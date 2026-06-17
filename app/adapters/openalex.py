"""OpenAlex adapter — broad open scholarly index with a clean JSON API.

Complements PubMed with citation counts, DOIs, and non-MEDLINE venues. Uses the
polite pool (an email in the `mailto` param) for better rate limits.
"""

from __future__ import annotations

import asyncio
from typing import List, Optional

import httpx

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.config import get_settings
from app.core.http import DiskCache, TokenBucket
from app.models.article import Article, Author, ContentLevel, SearchFilters

WORKS = "https://api.openalex.org/works"


@register
class OpenAlexAdapter(SourceAdapter):
    name = "openalex"
    label = "OpenAlex"
    group = "Indexed literature"
    kind = SourceKind.API
    default_on = True

    def __init__(self) -> None:
        self.email = get_settings().ncbi_tool_email
        self.bucket = TokenBucket(8.0)
        self.cache = DiskCache("openalex")

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        params = {
            "search": query,
            "per_page": str(min(filters.max_results_per_source, 50)),
            "sort": "relevance_score:desc",
        }
        date_filters = []
        if filters.year_from:
            date_filters.append(f"from_publication_date:{filters.year_from}-01-01")
        if filters.year_to:
            date_filters.append(f"to_publication_date:{filters.year_to}-12-31")
        if not filters.include_preprints:
            date_filters.append("type:article")
        if date_filters:
            params["filter"] = ",".join(date_filters)
        if self.email:
            params["mailto"] = self.email

        cache_key = str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            results = cached
        else:
            results = await self._fetch(params)
            self.cache.set(cache_key, results)

        return [self._parse(w) for w in results]

    async def _fetch(self, params: dict, retries: int = 2) -> list:
        """GET with backoff retry. OpenAlex's anonymous pool can be slow under load,
        so we use a generous timeout and retry transient timeouts/5xx."""
        headers = {"User-Agent": "OncoLit/0.1 (research literature tool)"}
        last_exc: Optional[Exception] = None
        for attempt in range(retries + 1):
            await self.bucket.acquire()
            try:
                async with httpx.AsyncClient(timeout=45.0, headers=headers) as client:
                    resp = await client.get(WORKS, params=params)
                    resp.raise_for_status()
                    return resp.json().get("results", [])
            except (httpx.TimeoutException, httpx.HTTPStatusError) as exc:
                last_exc = exc
                if isinstance(exc, httpx.HTTPStatusError) and exc.response.status_code < 500:
                    raise  # 4xx won't fix itself
                await asyncio.sleep(1.5 * (attempt + 1))
        raise last_exc  # type: ignore[misc]

    def _parse(self, w: dict) -> Article:
        doi = _strip_doi(w.get("doi"))
        pmid = _pmid(w.get("ids", {}))
        abstract = _reconstruct_abstract(w.get("abstract_inverted_index"))
        primary = w.get("primary_location") or {}
        venue = (primary.get("source") or {}).get("display_name")
        landing = primary.get("landing_page_url")
        is_preprint = w.get("type") == "preprint"

        authors = [
            Author(name=a["author"]["display_name"])
            for a in w.get("authorships", [])
            if a.get("author", {}).get("display_name")
        ]

        url = landing or (f"https://doi.org/{doi}" if doi else w.get("id"))
        return Article(
            doi=doi,
            pmid=pmid,
            source_id=w.get("id"),
            title=(w.get("title") or "(untitled)").strip(),
            authors=authors,
            venue=venue,
            year=w.get("publication_year"),
            pub_type=w.get("type"),
            is_preprint=is_preprint,
            abstract=abstract,
            content_level=(
                ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY
            ),
            source=self.name,
            url=url,
            citation_count=w.get("cited_by_count"),
        )


def _strip_doi(doi: Optional[str]) -> Optional[str]:
    if not doi:
        return None
    return doi.replace("https://doi.org/", "").strip() or None


def _pmid(ids: dict) -> Optional[str]:
    pmid_url = ids.get("pmid")
    if not pmid_url:
        return None
    return pmid_url.rstrip("/").split("/")[-1]


def _reconstruct_abstract(inverted: Optional[dict]) -> Optional[str]:
    """OpenAlex stores abstracts as an inverted index {word: [positions]}."""
    if not inverted:
        return None
    positions: List[tuple] = []
    for word, idxs in inverted.items():
        for i in idxs:
            positions.append((i, word))
    if not positions:
        return None
    positions.sort()
    return " ".join(word for _, word in positions)
