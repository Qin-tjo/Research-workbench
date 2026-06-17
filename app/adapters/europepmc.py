"""Europe PMC adapter via the REST search API.

Adds open-access full-text availability, preprints (source "PPR"), and citation
counts, complementing PubMed/OpenAlex. Uses resultType=core to get abstracts and
author lists in one call.
"""

from __future__ import annotations

import asyncio
from typing import List, Optional

import httpx

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.http import DiskCache, TokenBucket
from app.models.article import Article, Author, ContentLevel, SearchFilters

SEARCH = "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
_UA = {"User-Agent": "OncoLit/0.1 (research literature tool)"}


@register
class EuropePMCAdapter(SourceAdapter):
    name = "europepmc"
    label = "Europe PMC"
    group = "Indexed literature"
    kind = SourceKind.API
    default_on = True

    def __init__(self) -> None:
        self.bucket = TokenBucket(8.0)
        self.cache = DiskCache("europepmc")

    def _build_query(self, query: str, filters: SearchFilters) -> str:
        parts = [f"({query})"]
        if filters.year_from or filters.year_to:
            lo = filters.year_from or 1900
            hi = filters.year_to or 3000
            parts.append(f"(PUB_YEAR:[{lo} TO {hi}])")
        if not filters.include_preprints:
            parts.append("NOT (SRC:PPR)")
        return " AND ".join(parts)

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        params = {
            "query": self._build_query(query, filters),
            "format": "json",
            "resultType": "core",
            "pageSize": str(min(filters.max_results_per_source, 100)),
        }
        cache_key = str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            results = cached
        else:
            results = await self._fetch(params)
            self.cache.set(cache_key, results)
        return [self._parse(r) for r in results]

    async def _fetch(self, params: dict, retries: int = 2) -> list:
        last_exc: Optional[Exception] = None
        for attempt in range(retries + 1):
            await self.bucket.acquire()
            try:
                async with httpx.AsyncClient(timeout=45.0, headers=_UA) as client:
                    resp = await client.get(SEARCH, params=params)
                    resp.raise_for_status()
                    return resp.json().get("resultList", {}).get("result", [])
            except (httpx.TimeoutException, httpx.HTTPStatusError) as exc:
                last_exc = exc
                if isinstance(exc, httpx.HTTPStatusError) and exc.response.status_code < 500:
                    raise
                await asyncio.sleep(1.5 * (attempt + 1))
        raise last_exc  # type: ignore[misc]

    def _parse(self, r: dict) -> Article:
        source = r.get("source", "")
        rid = r.get("id")
        is_preprint = source == "PPR" or _has_preprint_type(r)
        abstract = r.get("abstractText")

        journal = (r.get("journalInfo") or {}).get("journal", {}).get("title")
        venue = journal or ("Preprint" if is_preprint else None)

        authors = [
            Author(name=n.strip())
            for n in (r.get("authorString") or "").split(",")
            if n.strip()
        ]

        doi = r.get("doi")
        url = _record_url(source, rid, doi)

        return Article(
            doi=doi,
            pmid=r.get("pmid"),
            source_id=f"{source}:{rid}" if rid else None,
            title=(r.get("title") or "(untitled)").strip(),
            authors=authors,
            venue=venue,
            year=_to_int(r.get("pubYear")),
            pub_type=_first_pub_type(r),
            is_preprint=is_preprint,
            abstract=abstract,
            content_level=(
                ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY
            ),
            source=self.name,
            url=url,
            citation_count=r.get("citedByCount"),
        )


def _has_preprint_type(r: dict) -> bool:
    return any("preprint" in t.lower() for t in _pub_types(r))


def _pub_types(r: dict) -> List[str]:
    return (r.get("pubTypeList") or {}).get("pubType", []) or []


def _first_pub_type(r: dict) -> Optional[str]:
    types = _pub_types(r)
    return types[0] if types else None


def _record_url(source: str, rid: Optional[str], doi: Optional[str]) -> Optional[str]:
    if source and rid:
        return f"https://europepmc.org/article/{source}/{rid}"
    if doi:
        return f"https://doi.org/{doi}"
    return None


def _to_int(value) -> Optional[int]:
    try:
        return int(value)
    except (TypeError, ValueError):
        return None
