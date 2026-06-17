"""Offline tests for dedup, ranking, and the report (no network, no LLM)."""

from __future__ import annotations

from app.models.article import Article, ContentLevel, RankedArticle, SearchFilters
from app.pipeline.dedup import dedup
from app.pipeline.rank import rank
from app.pipeline.search import _within_years, corpus_coverage, split_tiers


def _art(**kw) -> Article:
    base = dict(title="A study", source="pubmed", content_level=ContentLevel.ABSTRACT_ONLY)
    base.update(kw)
    return Article(**base)


def test_dedup_merges_by_doi_and_keeps_richest():
    a = _art(doi="10.1/x", title="KRAS study", source="pubmed", abstract="abs")
    b = _art(
        doi="10.1/X",  # case-insensitive
        title="KRAS study",
        source="openalex",
        abstract="abs full",
        full_text="full body",
        content_level=ContentLevel.FULL_TEXT,
        citation_count=42,
    )
    merged = dedup([a, b])
    assert len(merged) == 1
    assert merged[0].content_level == ContentLevel.FULL_TEXT
    assert merged[0].citation_count == 42


def test_dedup_fuzzy_title_year():
    a = _art(title="EGFR mutations in lung cancer", year=2023, source="pubmed")
    b = _art(title="EGFR mutations in lung cancer.", year=2023, source="openalex")
    assert len(dedup([a, b])) == 1


def test_dedup_keeps_distinct():
    a = _art(doi="10.1/a", title="Topic A")
    b = _art(doi="10.1/b", title="Completely different topic B")
    assert len(dedup([a, b])) == 2


def test_rank_orders_by_relevance_and_recency():
    relevant = _art(title="KRAS G12C inhibitor in NSCLC", year=2025, citation_count=500)
    irrelevant = _art(title="Unrelated dermatology review", year=2005)
    ranked = rank("KRAS G12C inhibitor NSCLC", [irrelevant, relevant])
    assert ranked[0].article.title == relevant.title
    assert ranked[0].score > ranked[1].score
    assert set(ranked[0].score_breakdown) == {"relevance", "recency", "citations", "source"}


def _ranked(title, level, score):
    return RankedArticle(article=_art(title=title, content_level=level), score=score)


def test_split_tiers_prefers_abstract_papers_and_caps():
    items = [
        _ranked("Paper A (abstract)", ContentLevel.ABSTRACT_ONLY, 0.9),
        _ranked("Paper B (title only)", ContentLevel.TITLE_ONLY, 0.85),
        _ranked("Paper C (abstract)", ContentLevel.ABSTRACT_ONLY, 0.8),
        _ranked("Paper D (full text)", ContentLevel.FULL_TEXT, 0.7),
    ]
    key, additional = split_tiers(items, key_count=2)
    # Key tier = top 2 abstract-bearing, in rank order; title-only is pushed to additional.
    assert [r.article.title for r in key] == ["Paper A (abstract)", "Paper C (abstract)"]
    titles = {r.article.title for r in additional}
    assert "Paper B (title only)" in titles and "Paper D (full text)" in titles


def test_within_years_enforces_range_keeps_unknown():
    arts = [
        _art(title="in range", year=2024),
        _art(title="too old", year=2019),
        _art(title="too new", year=2027),
        _art(title="no year", year=None),
    ]
    kept = _within_years(arts, SearchFilters(year_from=2022, year_to=2026))
    titles = {a.title for a in kept}
    assert "in range" in titles
    assert "no year" in titles  # unknown year is kept
    assert "too old" not in titles and "too new" not in titles


def test_within_years_noop_without_range():
    arts = [_art(title="x", year=1999)]
    assert _within_years(arts, SearchFilters()) == arts


def test_corpus_coverage_counts():
    items = [
        _ranked("a", ContentLevel.ABSTRACT_ONLY, 0.9),
        _ranked("b", ContentLevel.TITLE_ONLY, 0.8),
    ]
    items[0].article.year = 2024
    cov = corpus_coverage(items, analyzed=1)
    assert cov["total"] == 2
    assert cov["with_abstract"] == 1
    assert cov["title_only"] == 1
    assert cov["analyzed"] == 1
