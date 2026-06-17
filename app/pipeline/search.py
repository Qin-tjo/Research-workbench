"""Search orchestration: fan out to selected adapters, normalize, dedup, rank.

This is the spine of the pipeline. The summarization step (Part 4) consumes the
ranked output; the report renderer (Part 5) consumes the summaries.
"""

from __future__ import annotations

import asyncio
import logging
from typing import List, Optional, Tuple

from app.adapters import registry
from app.models.article import ContentLevel, RankedArticle, SearchFilters
from app.pipeline.dedup import dedup
from app.pipeline.rank import rank

logger = logging.getLogger(__name__)

_HAS_TEXT = {ContentLevel.ABSTRACT_ONLY, ContentLevel.FULL_TEXT}


def split_tiers(
    ranked: List[RankedArticle], key_count: int
) -> Tuple[List[RankedArticle], List[RankedArticle]]:
    """Split ranked results into a deep-analysis tier and an 'additional' tier.

    Key tier = the top `key_count` records that have an abstract (in rank order); only
    these are worth per-paper TL;DR + extraction. Everything else — lower-ranked abstract
    papers plus all title-only records (feeds/conferences/metadata) — is the additional
    tier, listed compactly without LLM calls. Additional preserves rank order.
    """
    key: List[RankedArticle] = []
    additional: List[RankedArticle] = []
    for r in ranked:  # ranked is already sorted by score, descending
        if len(key) < key_count and r.article.content_level in _HAS_TEXT:
            key.append(r)
        else:
            additional.append(r)
    return key, additional


def corpus_coverage(ranked: List[RankedArticle], analyzed: int) -> dict:
    """Stats for the report's coverage note / executive stats strip."""
    with_abstract = sum(1 for r in ranked if r.article.content_level in _HAS_TEXT)
    years = [r.article.year for r in ranked if r.article.year]
    return {
        "total": len(ranked),
        "analyzed": analyzed,
        "with_abstract": with_abstract,
        "title_only": len(ranked) - with_abstract,
        "year_min": min(years) if years else None,
        "year_max": max(years) if years else None,
    }


async def run_search(
    query: str, filters: SearchFilters
) -> Tuple[List[RankedArticle], List[str]]:
    """Execute a full search.

    Returns (ranked articles, warnings). Adapter failures are isolated: one
    source erroring never sinks the whole search.
    """
    adapters = registry.get_adapters(filters.sources)
    if not adapters:
        return [], ["No valid sources selected."]

    warnings: List[str] = []

    async def _run(adapter) -> list:
        try:
            return await adapter.search(query, filters)
        except Exception as exc:  # isolate per-source failures
            detail = str(exc) or exc.__class__.__name__
            logger.warning("Adapter %s failed: %s", adapter.name, detail, exc_info=True)
            warnings.append(f"Source '{adapter.label}' failed: {detail}")
            return []

    results = await asyncio.gather(*[_run(a) for a in adapters])
    flat = [art for batch in results for art in batch]

    # Enforce the year range uniformly. Some sources (ClinicalTrials.gov, openFDA) have no
    # server-side date filter, so without this guard their out-of-range records leak in and
    # the report's year range no longer matches the requested window.
    flat = _within_years(flat, filters)

    if not flat:
        warnings.append("No results found for this query.")
        return [], warnings

    merged = dedup(flat)
    ranked = rank(query, merged, prioritize_citations=filters.prioritize_citations)
    return ranked, warnings


def _within_years(articles: list, filters: SearchFilters) -> list:
    """Drop records whose (known) year falls outside the requested range.

    Records with no parsed year are kept — they don't violate a stated range and aren't
    counted in the report's displayed year span, so they can't cause the mismatch.
    """
    if filters.year_from is None and filters.year_to is None:
        return articles
    lo = filters.year_from or 0
    hi = filters.year_to or 9999
    return [a for a in articles if a.year is None or lo <= a.year <= hi]


def resolve_filters(
    sources: Optional[List[str]],
    year_from: Optional[int],
    year_to: Optional[int],
    max_per_source: int,
    include_preprints: bool,
) -> SearchFilters:
    selected = sources or registry.default_source_names()
    return SearchFilters(
        sources=selected,
        year_from=year_from,
        year_to=year_to,
        max_results_per_source=max_per_source,
        include_preprints=include_preprints,
    )
