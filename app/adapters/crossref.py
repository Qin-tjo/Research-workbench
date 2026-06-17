"""Crossref adapter — canonical DOI metadata across publishers.

Also exposes `query_works` + `parse_item`, reused by the preprint adapter since
bioRxiv/medRxiv are reachable through Crossref's `type:posted-content` records.
"""

from __future__ import annotations

import re
from typing import List, Optional

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.config import get_settings
from app.core.http import DiskCache, TokenBucket, get_json
from app.models.article import Article, Author, ContentLevel, SearchFilters

WORKS = "https://api.crossref.org/works"

# Shared across the Crossref, preprint, and four conference adapters, so keep it
# conservative — the anonymous Crossref pool rate-limits bursts (HTTP 429).
_bucket = TokenBucket(4.0)
_cache = DiskCache("crossref")
_JATS_TAG = re.compile(r"<[^>]+>")


def _ua_headers() -> dict:
    # Crossref's "polite pool" keys off a mailto in the User-Agent.
    email = get_settings().ncbi_tool_email
    ua = "OncoLit/0.1 (research literature tool"
    ua += f"; mailto:{email})" if email else ")"
    return {"User-Agent": ua}


async def query_works(params: dict) -> List[dict]:
    """Shared Crossref GET with caching. Returns message.items."""
    cache_key = str(sorted(params.items()))
    cached = _cache.get(cache_key)
    if cached is not None:
        return cached
    data = await get_json(WORKS, params, _bucket, headers=_ua_headers())
    items = (data or {}).get("message", {}).get("items", [])
    _cache.set(cache_key, items)
    return items


def _strip_jats(text: Optional[str]) -> Optional[str]:
    if not text:
        return None
    return _JATS_TAG.sub("", text).strip() or None


def parse_item(item: dict, source: str, *, is_preprint: bool = False) -> Article:
    doi = item.get("DOI")
    title_list = item.get("title") or []
    title = _strip_jats(title_list[0]) if title_list else None
    title = title or "(untitled)"
    venue_list = item.get("container-title") or []
    venue = venue_list[0] if venue_list else None
    if not venue and is_preprint:
        insts = item.get("institution") or []
        venue = insts[0].get("name") if insts else "Preprint"

    issued = (item.get("issued") or {}).get("date-parts") or [[None]]
    year = issued[0][0] if issued and issued[0] else None

    authors = []
    for a in item.get("author", []) or []:
        name = " ".join(p for p in [a.get("given"), a.get("family")] if p)
        if name:
            authors.append(Author(name=name))

    abstract = _strip_jats(item.get("abstract"))
    url = item.get("URL") or (f"https://doi.org/{doi}" if doi else None)

    return Article(
        doi=doi,
        source_id=doi,
        title=title,
        authors=authors,
        venue=venue,
        year=year,
        pub_type=item.get("type"),
        is_preprint=is_preprint,
        abstract=abstract,
        content_level=ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY,
        source=source,
        url=url,
        citation_count=item.get("is-referenced-by-count"),
    )


def date_filter(filters: SearchFilters) -> Optional[str]:
    parts = []
    if filters.year_from:
        parts.append(f"from-pub-date:{filters.year_from}-01-01")
    if filters.year_to:
        parts.append(f"until-pub-date:{filters.year_to}-12-31")
    return ",".join(parts) if parts else None


@register
class CrossrefAdapter(SourceAdapter):
    name = "crossref"
    label = "Crossref"
    group = "Indexed literature"
    kind = SourceKind.API
    default_on = True

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        params = {"query": query, "rows": str(min(filters.max_results_per_source, 50))}
        df = date_filter(filters)
        if df:
            params["filter"] = df
        items = await query_works(params)
        return [parse_item(i, self.name) for i in items]
