"""openFDA adapter — FDA drug labels (indications).

Surfaces approved-drug context (indications and usage) relevant to an oncology
query. Maps a structured product label to the canonical Article shape, linking to
the full label on DailyMed.
"""

from __future__ import annotations

from typing import List, Optional

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.http import DiskCache, TokenBucket, get_json
from app.models.article import Article, ContentLevel, SearchFilters

LABEL = "https://api.fda.gov/drug/label.json"
_MAX_INDICATION_CHARS = 1500


@register
class OpenFDAAdapter(SourceAdapter):
    name = "openfda"
    label = "openFDA (drug labels)"
    group = "Trials & regulatory"
    kind = SourceKind.API
    default_on = True

    def __init__(self) -> None:
        self.bucket = TokenBucket(4.0)
        self.cache = DiskCache("openfda")

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        # Search the indications field; quote terms so Lucene treats it as a phrase-ish OR.
        terms = " ".join(query.split())
        params = {
            "search": f'indications_and_usage:"{terms}"',
            "limit": str(min(filters.max_results_per_source, 50)),
        }
        cache_key = str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            results = cached
        else:
            # openFDA returns 404 when nothing matches; get_json maps that to None.
            data = await get_json(LABEL, params, self.bucket)
            results = (data or {}).get("results", []) if data else []
            self.cache.set(cache_key, results)
        return [self._parse(r) for r in results]

    def _parse(self, r: dict) -> Article:
        openfda = r.get("openfda", {})
        brand = _first(openfda.get("brand_name"))
        generic = _first(openfda.get("generic_name"))
        name = brand or generic or "Drug label"
        title = f"{name} — FDA label" + (f" ({generic})" if generic and brand else "")

        indications = _first(r.get("indications_and_usage"))
        abstract = indications[:_MAX_INDICATION_CHARS] if indications else None

        spl_set_id = _first(openfda.get("spl_set_id"))
        url = (
            f"https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid={spl_set_id}"
            if spl_set_id
            else None
        )
        year = _year(r.get("effective_time"))

        return Article(
            source_id=r.get("id"),
            title=title,
            venue="FDA drug label",
            year=year,
            pub_type="Drug label",
            abstract=abstract,
            content_level=ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY,
            source=self.name,
            url=url,
        )


def _first(value) -> Optional[str]:
    if isinstance(value, list):
        return value[0] if value else None
    return value


def _year(effective_time: Optional[str]) -> Optional[int]:
    if effective_time and len(effective_time) >= 4 and effective_time[:4].isdigit():
        return int(effective_time[:4])
    return None
