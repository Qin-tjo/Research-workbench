"""Conference / society adapters: ASCO, AACR, ESMO, ASH.

Why not scrape the society sites directly? Their search endpoints are both
disallowed by robots.txt (`Disallow: /search`, `/search-results`) and gated
behind a Cloudflare bot challenge (HTTP 403). Honoring robots and not bypassing
bot protection (per the project plan), we reach the same content the legitimate
way: each society's meeting abstracts and papers publish in a specific journal,
which Crossref indexes by ISSN — often ahead of PubMed's MEDLINE indexing, so we
keep much of the freshness benefit. We flag meeting-abstract records via their
title patterns.

- ASCO  -> Journal of Clinical Oncology (meeting abstracts = JCO supplements)
- AACR  -> Cancer Research            (meeting abstracts = "Abstract N:" supplements)
- ESMO  -> Annals of Oncology         (congress abstracts = "NNNP/PD/O" numbers)
- ASH   -> Blood                      (annual-meeting abstracts = Blood supplements)
"""

from __future__ import annotations

import re
from typing import List

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.crossref import date_filter, parse_item, query_works
from app.adapters.registry import register
from app.models.article import Article, SearchFilters

# Meeting-abstract title signatures, by society.
_AACR_ABSTRACT = re.compile(r"^\s*Abstract\s+[\w-]+\s*[:.]", re.IGNORECASE)
_ESMO_ABSTRACT = re.compile(r"^\s*\d+[A-Z]{1,3}\b")  # e.g. 205P, 397PD, 1O
_ASCO_ABSTRACT = re.compile(r"\babstract\b", re.IGNORECASE)


class _ConferenceAdapter(SourceAdapter):
    """Base: venue-scoped Crossref search for one society."""

    kind = SourceKind.API
    group = "Conferences"
    default_on = True

    issns: List[str] = []
    venue_label: str = ""  # e.g. "AACR (Cancer Research)"
    _abstract_pattern: "re.Pattern" = _ASCO_ABSTRACT

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        issn_filter = ",".join(f"issn:{i}" for i in self.issns)
        params = {
            "query.bibliographic": query,
            "rows": str(min(filters.max_results_per_source, 50)),
            "filter": issn_filter,
        }
        df = date_filter(filters)
        if df:
            params["filter"] += "," + df

        items = await query_works(params)
        out: List[Article] = []
        for item in items:
            art = parse_item(item, self.name)
            art.is_conference = self._looks_like_abstract(art.title)
            if self.venue_label:
                art.conference = self.venue_label
            out.append(art)
        return out

    def _looks_like_abstract(self, title: str) -> bool:
        return bool(self._abstract_pattern.search(title or ""))


@register
class ASCOAdapter(_ConferenceAdapter):
    name = "asco"
    label = "ASCO (JCO)"
    issns = ["1527-7755", "0732-183X"]
    venue_label = "ASCO / Journal of Clinical Oncology"
    _abstract_pattern = _ASCO_ABSTRACT


@register
class AACRAdapter(_ConferenceAdapter):
    name = "aacr"
    label = "AACR (Cancer Research)"
    issns = ["1538-7445", "0008-5472"]
    venue_label = "AACR / Cancer Research"
    _abstract_pattern = _AACR_ABSTRACT


@register
class ESMOAdapter(_ConferenceAdapter):
    name = "esmo"
    label = "ESMO (Annals of Oncology)"
    issns = ["1569-8041", "0923-7534"]
    venue_label = "ESMO / Annals of Oncology"
    _abstract_pattern = _ESMO_ABSTRACT


@register
class ASHAdapter(_ConferenceAdapter):
    name = "ash"
    label = "ASH (Blood)"
    issns = ["1528-0020", "0006-4971"]
    venue_label = "ASH / Blood"
    _abstract_pattern = _ASCO_ABSTRACT
