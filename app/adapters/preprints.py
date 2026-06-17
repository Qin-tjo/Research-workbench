"""bioRxiv / medRxiv preprint adapter.

Reachable via Crossref's `type:posted-content` records; we keep only items whose
depositing institution is bioRxiv or medRxiv. Keyword-searchable and reliable,
sharing the Crossref query path.
"""

from __future__ import annotations

from typing import List

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.crossref import date_filter, parse_item, query_works
from app.adapters.registry import register
from app.models.article import Article, SearchFilters

_SERVERS = {"biorxiv", "medrxiv"}


def _is_target_server(item: dict) -> bool:
    insts = item.get("institution") or []
    names = {(i.get("name") or "").lower() for i in insts}
    if names & _SERVERS:
        return True
    # Fallback: bioRxiv/medRxiv DOIs use the date-based 10.1101/YYYY.MM.DD pattern.
    doi = (item.get("DOI") or "").lower()
    return doi.startswith("10.1101/20")


@register
class PreprintAdapter(SourceAdapter):
    name = "preprints"
    label = "bioRxiv / medRxiv"
    group = "Preprints"
    kind = SourceKind.API
    default_on = True

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        # Over-fetch a little, since we filter down to the two target servers.
        rows = min(filters.max_results_per_source * 3, 100)
        params = {
            "query.bibliographic": query,
            "rows": str(rows),
            "filter": "type:posted-content",
        }
        df = date_filter(filters)
        if df:
            params["filter"] += "," + df

        items = await query_works(params)
        out: List[Article] = []
        for item in items:
            if not _is_target_server(item):
                continue
            out.append(parse_item(item, self.name, is_preprint=True))
            if len(out) >= filters.max_results_per_source:
                break
        return out
