"""CLI render path: run.json + analysis.json -> interactive HTML (no network)."""

from __future__ import annotations

import json

from app.cli import main
from app.models.article import Article, ContentLevel


def _run_json():
    art = Article(
        doi="10.1/a",
        title="Adagrasib in KRAS G12C NSCLC",
        authors=[{"name": "J Smith"}],
        venue="JCO",
        year=2024,
        abstract="Adagrasib showed activity.",
        content_level=ContentLevel.ABSTRACT_ONLY,
        source="pubmed",
        url="https://pubmed.ncbi.nlm.nih.gov/1/",
    )
    extra = Article(
        title="A conference abstract",
        venue="AACR",
        year=2024,
        content_level=ContentLevel.TITLE_ONLY,
        source="aacr",
        url="https://doi.org/10.1/x",
        is_conference=True,
    )
    return {
        "query": "KRAS G12C inhibitors",
        "sources": ["pubmed", "aacr"],
        "coverage": {"total": 2, "analyzed": 1, "with_abstract": 1, "title_only": 1,
                     "year_min": 2024, "year_max": 2024},
        "warnings": [],
        "key_papers": [{"id": 1, "score": 0.9, "article": art.model_dump(mode="json")}],
        "additional": [{"score": 0.3, "article": extra.model_dump(mode="json")}],
        "additional_total": 1,
    }


def _write(tmp_path, run, analysis):
    rp = tmp_path / "run.json"
    ap = tmp_path / "analysis.json"
    rp.write_text(json.dumps(run), "utf-8")
    ap.write_text(json.dumps(analysis), "utf-8")
    return rp, ap


def test_render_science_first_report_no_table(tmp_path):
    analysis = {
        "articles": [{"id": 1, "tldr": "Adagrasib was active in NSCLC."}],
        "synthesis": {
            "executive_summary": ["Adagrasib shows activity [1]."],
            "themes": [{"heading": "Efficacy", "body": "Responses were seen [1]."}],
        },
    }
    rp, ap = _write(tmp_path, _run_json(), analysis)
    out = tmp_path / "report.html"
    assert main(["render", str(rp), str(ap), "-o", str(out)]) == 0
    html = out.read_text("utf-8")
    assert "Key findings" in html and "Efficacy" in html
    assert html.count('href="#art-1"') >= 2 and 'id="art-1"' in html
    assert "Adagrasib was active in NSCLC." in html
    assert "Comparison" not in html  # no table provided -> omitted
    assert "<svg" not in html  # no stats charts
    assert 'id="filter"' in html and 'class="chip"' in html  # interactive chrome
    assert "Additional references" in html and "A conference abstract" in html
    assert "anthropic" not in html.lower()


def test_render_includes_table_when_provided(tmp_path):
    analysis = {
        "articles": [{"id": 1, "tldr": "x"}],
        "synthesis": {"executive_summary": [], "themes": []},
        "table": {
            "caption": "Agents",
            "columns": ["drug", "target"],
            "rows": [{"id": 1, "cells": {"drug": "adagrasib", "target": "KRAS G12C"}}],
        },
    }
    rp, ap = _write(tmp_path, _run_json(), analysis)
    out = tmp_path / "report.html"
    assert main(["render", str(rp), str(ap), "-o", str(out)]) == 0
    html = out.read_text("utf-8")
    assert "Comparison" in html and "adagrasib" in html and "KRAS G12C" in html


def test_render_tolerates_missing_analysis_ids(tmp_path):
    analysis = {"articles": [{"id": 99, "tldr": "x"}],
                "synthesis": {"executive_summary": [], "themes": []}}
    rp, ap = _write(tmp_path, _run_json(), analysis)
    out = tmp_path / "report.html"
    assert main(["render", str(rp), str(ap), "-o", str(out)]) == 0
    html = out.read_text("utf-8")
    # Unmatched key paper falls back to its abstract for the card text.
    assert "Adagrasib showed activity." in html
