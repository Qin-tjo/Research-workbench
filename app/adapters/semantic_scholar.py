"""Semantic Scholar adapter — citation graph signals + TLDRs.

Adds influential-citation counts and one-line TLDRs useful for ranking. The
public API is heavily rate-limited without a key; set S2_API_KEY to raise limits.
Failures are isolated by the pipeline, so a 429 degrades gracefully.
"""

from __future__ import annotations

from typing import List

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.config import get_settings
from app.core.http import DiskCache, TokenBucket, get_json
from app.models.article import Article, Author, ContentLevel, SearchFilters

SEARCH = "https://api.semanticscholar.org/graph/v1/paper/search"
_FIELDS = (
    "title,abstract,year,venue,citationCount,influentialCitationCount,"
    "externalIds,tldr,authors,publicationTypes"
)


@register
class SemanticScholarAdapter(SourceAdapter):
    name = "semanticscholar"
    label = "Semantic Scholar"
    group = "Indexed literature"
    kind = SourceKind.API
    default_on = False  # rate-limited without a key; opt-in

    def __init__(self) -> None:
        self.api_key = get_settings().s2_api_key
        # Conservative without a key (public limit is ~1 rps shared).
        self.bucket = TokenBucket(5.0 if self.api_key else 1.0)
        self.cache = DiskCache("semanticscholar")

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        params = {
            "query": query,
            "limit": str(min(filters.max_results_per_source, 100)),
            "fields": _FIELDS,
        }
        if filters.year_from or filters.year_to:
            lo = filters.year_from or ""
            hi = filters.year_to or ""
            params["year"] = f"{lo}-{hi}"

        cache_key = str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            results = cached
        else:
            headers = {"x-api-key": self.api_key} if self.api_key else None
            data = await get_json(SEARCH, params, self.bucket, headers=headers)
            results = (data or {}).get("data", [])
            self.cache.set(cache_key, results)
        return [self._parse(r) for r in results]

    def _parse(self, r: dict) -> Article:
        ext = r.get("externalIds") or {}
        doi = ext.get("DOI")
        pmid = str(ext["PubMed"]) if ext.get("PubMed") else None
        tldr = (r.get("tldr") or {}).get("text")
        abstract = r.get("abstract") or tldr

        authors = [
            Author(name=a["name"]) for a in (r.get("authors") or []) if a.get("name")
        ]
        pub_types = r.get("publicationTypes") or []
        url = (
            f"https://doi.org/{doi}"
            if doi
            else f"https://www.semanticscholar.org/paper/{r.get('paperId')}"
        )
        return Article(
            doi=doi,
            pmid=pmid,
            source_id=r.get("paperId"),
            title=(r.get("title") or "(untitled)").strip(),
            authors=authors,
            venue=r.get("venue") or None,
            year=r.get("year"),
            pub_type=pub_types[0] if pub_types else None,
            abstract=abstract,
            content_level=ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY,
            source=self.name,
            url=url,
            citation_count=r.get("citationCount"),
        )
