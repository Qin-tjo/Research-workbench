"""Best-effort open-access full-text enrichment for the key papers.

For the small key tier only (bounded cost), resolve each paper to a PubMed Central
ID via Europe PMC (by PMID or DOI) and, when it's open access in Europe PMC, pull
the full text from `PMC/{pmcid}/fullTextXML`. Retrieved papers are upgraded to
FULL_TEXT so the analysis works from the whole paper. Paywalled papers stay
abstract-level — there's no legal full text to fetch.
"""

from __future__ import annotations

import asyncio
import logging
import re
from typing import List, Optional

import httpx

from app.core.http import TokenBucket
from app.models.article import Article, ContentLevel

logger = logging.getLogger(__name__)

_SEARCH = "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
_FULLTEXT = "https://www.ebi.ac.uk/europepmc/webservices/rest/{pmcid}/fullTextXML"
_UA = {"User-Agent": "OncoLit/0.1 (research literature tool)"}
_TAG = re.compile(r"<[^>]+>")
_WS = re.compile(r"\s+")
_MAX_CHARS = 40000
_MIN_USEFUL = 1500  # below this it's front-matter, not real full text

_bucket = TokenBucket(6.0)


def _query_for(art: Article) -> Optional[str]:
    if art.pmid:
        return f"EXT_ID:{art.pmid} AND SRC:MED"
    if art.doi:
        return f'DOI:"{art.doi}"'
    return None


def _direct_pmcid(art: Article) -> Optional[str]:
    """europepmc-sourced records may already carry a PMC id in source_id."""
    sid = art.source_id or ""
    if sid.upper().startswith("PMC:"):
        return sid.split(":", 1)[1]
    return None


async def _resolve_pmcid(client: httpx.AsyncClient, art: Article) -> Optional[str]:
    direct = _direct_pmcid(art)
    if direct:
        return direct
    query = _query_for(art)
    if not query:
        return None
    await _bucket.acquire()
    resp = await client.get(
        _SEARCH, params={"query": query, "format": "json", "resultType": "lite", "pageSize": "1"}
    )
    if resp.status_code != 200:
        return None
    results = resp.json().get("resultList", {}).get("result", [])
    if not results:
        return None
    rec = results[0]
    if rec.get("inEPMC") == "Y" and rec.get("pmcid"):
        return rec["pmcid"]
    return None


def _xml_to_text(xml: str) -> Optional[str]:
    m = re.search(r"<body[ >].*?</body>", xml, re.S)
    chunk = m.group(0) if m else xml
    text = _WS.sub(" ", _TAG.sub(" ", chunk)).strip()
    return text[:_MAX_CHARS] if len(text) >= _MIN_USEFUL else None


async def _fetch_one(client: httpx.AsyncClient, art: Article) -> None:
    pmcid = await _resolve_pmcid(client, art)
    if not pmcid:
        return
    await _bucket.acquire()
    resp = await client.get(_FULLTEXT.format(pmcid=pmcid))
    if resp.status_code != 200 or "<body" not in resp.text:
        return
    text = _xml_to_text(resp.text)
    if text:
        art.full_text = text
        art.content_level = ContentLevel.FULL_TEXT


async def enrich_fulltext(articles: List[Article], concurrency: int = 5) -> int:
    """Mutate articles in place, fetching OA full text where available. Returns count upgraded."""
    sem = asyncio.Semaphore(concurrency)
    before = sum(1 for a in articles if a.content_level == ContentLevel.FULL_TEXT)

    async with httpx.AsyncClient(timeout=45.0, headers=_UA, follow_redirects=True) as client:
        async def _guarded(a: Article) -> None:
            async with sem:
                try:
                    await _fetch_one(client, a)
                except Exception as exc:  # never let enrichment sink the run
                    logger.warning("full-text fetch failed for %s: %s", a.title[:50], exc)

        await asyncio.gather(*[_guarded(a) for a in articles])

    after = sum(1 for a in articles if a.content_level == ContentLevel.FULL_TEXT)
    return after - before
