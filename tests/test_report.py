"""Report rendering + guardrail tests (no network, no LLM)."""

from __future__ import annotations

import re

from app.analysis import (
    ArticleSummary,
    ComparisonTable,
    SynthesisResult,
    TableRow,
    Theme,
    article_key,
)
from app.models.article import Article, ContentLevel, RankedArticle
from app.report.renderer import render_report
from app.tone import find_tone_violations


def _summary_for(article, based_on="abstract"):
    return ArticleSummary(
        article_id=article_key(article),
        tldr="Plain grounded summary.",
        based_on=based_on,
    )


def _key_papers():
    a = Article(
        doi="10.1/a",
        title="KRAS G12C inhibitor trial",
        venue="JCO",
        year=2025,
        abstract="abs",
        content_level=ContentLevel.ABSTRACT_ONLY,
        source="pubmed",
        url="https://pubmed.ncbi.nlm.nih.gov/40000001/",
        citation_count=12,
    )
    return [RankedArticle(article=a, score=0.9)]


def _synthesis():
    return SynthesisResult(
        executive_summary=["Adagrasib shows activity in NSCLC [1]."],
        themes=[Theme(heading="Efficacy", body="Responses were observed [1].")],
    )


def test_report_is_self_contained_and_links_primary_source():
    ranked = _key_papers()
    summaries = [_summary_for(ranked[0].article)]
    html = render_report(
        "KRAS inhibitors",
        ranked,
        summaries,
        _synthesis(),
        sources=["pubmed"],
        coverage={"total": 1, "analyzed": 1, "with_abstract": 1, "title_only": 0,
                  "year_min": 2025, "year_max": 2025},
    )
    assert "<script src=" not in html
    assert "cdn" not in html.lower()
    assert "https://pubmed.ncbi.nlm.nih.gov/40000001/" in html
    # Citation markers in BOTH key findings and theme body linkify to the card.
    assert html.count('href="#art-1"') >= 2
    assert 'id="art-1"' in html
    assert "abstract" in html
    assert "Key findings" in html and "Efficacy" in html
    # No generic stats charts are emitted.
    assert "Publications per year" not in html and "<svg" not in html


def test_table_omitted_when_not_provided_and_rendered_when_useful():
    ranked = _key_papers()
    summaries = [_summary_for(ranked[0].article)]
    # No table -> no Comparison section.
    html = render_report("q", ranked, summaries, _synthesis(), sources=["pubmed"])
    assert "Comparison" not in html
    # With a useful table -> rendered, linking rows to cards.
    table = ComparisonTable(
        caption="Drugs vs mechanism",
        columns=["drug", "mechanism"],
        rows=[TableRow(id=1, cells={"drug": "adagrasib", "mechanism": "covalent G12C"})],
    )
    html2 = render_report("q", ranked, summaries, _synthesis(), table=table, sources=["pubmed"])
    assert "Comparison" in html2 and "adagrasib" in html2
    assert 'href="#art-1"' in html2


def test_link_integrity_every_card_has_primary_link():
    ranked = _key_papers()
    summaries = [_summary_for(ranked[0].article)]
    html = render_report("q", ranked, summaries, SynthesisResult(), sources=["pubmed"])
    cards = re.findall(r'<article class="card[^"]*"[^>]*>.*?</article>', html, re.S)
    assert cards
    for card in cards:
        assert "href=" in card


def test_full_text_badge_shows_when_available():
    ranked = _key_papers()
    ranked[0].article.content_level = ContentLevel.FULL_TEXT
    summaries = [_summary_for(ranked[0].article, based_on="full text")]
    html = render_report("q", ranked, summaries, SynthesisResult(), sources=["pubmed"])
    assert "full text" in html


def test_additional_tier_listed_without_card():
    key = _key_papers()
    extra = Article(
        title="A title-only conference abstract", venue="AACR", year=2024,
        content_level=ContentLevel.TITLE_ONLY, source="aacr",
        url="https://doi.org/10.1/x", is_conference=True,
    )
    html = render_report(
        "q", key, [_summary_for(key[0].article)], SynthesisResult(),
        additional=[RankedArticle(article=extra, score=0.3)], additional_total=1,
        sources=["pubmed", "aacr"],
    )
    assert "Additional references" in html
    assert "A title-only conference abstract" in html
    assert 'id="art-2"' not in html  # additional items get no per-paper card


def test_tone_lint_flags_ai_tells():
    assert find_tone_violations("This groundbreaking study will delve into the data.")
    assert not find_tone_violations("Overall survival improved by 4 months in the treatment arm.")
