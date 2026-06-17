"""Assemble the standalone HTML literature report (v4: science-first).

Sections: executive summary -> thematic synthesis -> key papers -> optional
comparison table (only when it supports the science) -> references -> additional.
No generic "paper stats" charts.
"""

from __future__ import annotations

import datetime
import html
import re
from pathlib import Path
from typing import List, Optional

from jinja2 import Environment, FileSystemLoader, select_autoescape

from app.models.article import RankedArticle
from app.pipeline.analysis import ArticleSummary, ComparisonTable, SynthesisResult, article_key

_TEMPLATE_DIR = Path(__file__).parent / "templates"
_env = Environment(
    loader=FileSystemLoader(str(_TEMPLATE_DIR)),
    autoescape=select_autoescape(["html"]),
)

_CITATION_RE = re.compile(r"\[(\d+)\]")


def _linkify(text: str, n: int) -> str:
    """Escape text and turn [k] markers (1..n) into anchor links to article cards."""
    escaped = html.escape(text or "")

    def repl(m: "re.Match") -> str:
        k = int(m.group(1))
        return f'<a class="cite" href="#art-{k}">[{k}]</a>' if 1 <= k <= n else m.group(0)

    return _CITATION_RE.sub(repl, escaped)


def _linkify_paragraphs(text: str, n: int) -> str:
    linked = _linkify(text, n)
    paras = [p.strip() for p in linked.split("\n\n") if p.strip()]
    return "".join(f"<p>{p}</p>" for p in paras)


def render_report(
    query: str,
    key_papers: List[RankedArticle],
    summaries: List[ArticleSummary],
    synthesis: SynthesisResult,
    *,
    table: Optional[ComparisonTable] = None,
    additional: Optional[List[RankedArticle]] = None,
    additional_total: int = 0,
    sources: List[str],
    coverage: Optional[dict] = None,
    warnings: Optional[List[str]] = None,
    model_note: str = "",
) -> str:
    additional = additional or []
    n = len(key_papers)
    by_id = {s.article_id: s for s in summaries}

    cards = []
    for i, r in enumerate(key_papers, start=1):
        a = r.article
        s = by_id.get(article_key(a))
        cards.append(
            {
                "idx": i,
                "article": a,
                "based_on": s.based_on if s else "unknown",
                "tldr": s.tldr if s else (a.abstract or a.title or ""),
            }
        )

    # Optional comparison table: only render when the analysis provided a useful one.
    table_data = None
    if table is not None and table.is_useful():
        title_by_id = {i: r.article for i, r in enumerate(key_papers, start=1)}
        rows = []
        for row in table.rows:
            art = title_by_id.get(row.id)
            if not art:
                continue
            rows.append(
                {"idx": row.id, "title": art.title,
                 "cells": [row.cells.get(c, "—") for c in table.columns]}
            )
        if rows:
            table_data = {"caption": table.caption, "columns": table.columns, "rows": rows}

    exec_summary = [_linkify(b, n) for b in synthesis.executive_summary]
    themes = [
        {"heading": t.heading, "body": _linkify_paragraphs(t.body, n)}
        for t in synthesis.themes
    ]

    template = _env.get_template("report.html")
    return template.render(
        query=query,
        generated_at=datetime.datetime.now().strftime("%Y-%m-%d %H:%M"),
        sources=sources,
        cards=cards,
        table=table_data,
        additional=[r.article for r in additional],
        additional_total=additional_total,
        exec_summary=exec_summary,
        themes=themes,
        coverage=coverage or {},
        warnings=warnings or [],
        model_note=model_note,
    )
