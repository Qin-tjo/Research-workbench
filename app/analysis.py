"""Analysis data models — the contract between the search step, the Claude Code
session that writes the analysis, and the render step.

This module has NO LLM/network dependency. The actual summarization/extraction/
synthesis is produced by the Claude Code session (see .claude/skills/litsearch),
which writes an `analysis.json` validated here by the `Analysis` model.
"""

from __future__ import annotations

from typing import Dict, List, Optional

from pydantic import BaseModel, Field

from app.models.article import Article, ContentLevel
from app.tone import banned_list_for_prompt

NOT_REPORTED = "Not reported"

#: Fallback comparison-table columns when the session doesn't propose query-specific ones.
DEFAULT_COLUMNS = [
    "cancer_type",
    "population",
    "intervention",
    "sample_size",
    "primary_endpoint",
    "key_result",
]

#: Rules the analysis step must follow — surfaced verbatim in the skill.
GROUNDING_RULES = (
    "Base every statement strictly on the provided text for that article. If a requested "
    f'value is not stated in the text, output exactly "{NOT_REPORTED}". Never infer, '
    "estimate, or use outside knowledge. Quote numbers exactly as written."
)
TONE_RULES = (
    "Write in plain, precise, declarative scientific prose, as a researcher briefing a "
    "colleague. No marketing or hype language, no first-person assistant voice, no hedging "
    f"boilerplate. Never use these phrases: {banned_list_for_prompt()}."
)


# --- Models consumed by the renderer -------------------------------------------------

class ArticleSummary(BaseModel):
    """A per-article summary bound to its source article for linking + provenance."""

    article_id: str
    tldr: str
    based_on: str  # "abstract" | "full text" | "title only"
    tone_flags: List[str] = Field(default_factory=list)


class Theme(BaseModel):
    """One thematic section of the synthesis. `body` carries inline [n] citations."""

    heading: str
    body: str


class SynthesisResult(BaseModel):
    """Structured cross-paper synthesis. Citation markers [n] index the key-paper list."""

    executive_summary: List[str] = Field(default_factory=list)
    themes: List[Theme] = Field(default_factory=list)


# --- The session-authored analysis file ----------------------------------------------

class AnalysisArticle(BaseModel):
    """One key paper's analysis, keyed by the integer `id` from run.json."""

    id: int
    tldr: str = ""


class TableRow(BaseModel):
    """A comparison-table row. `id` links to the key paper; `cells` maps column->value."""

    id: int
    cells: Dict[str, str] = Field(default_factory=dict)


class ComparisonTable(BaseModel):
    """An OPTIONAL table — included only when it supports the science, not paper stats."""

    caption: str = ""
    columns: List[str] = Field(default_factory=list)
    rows: List[TableRow] = Field(default_factory=list)

    def is_useful(self) -> bool:
        return bool(self.columns and self.rows)


class Analysis(BaseModel):
    """Validated shape of analysis.json (written by the Claude Code session)."""

    articles: List[AnalysisArticle] = Field(default_factory=list)
    synthesis: SynthesisResult = Field(default_factory=SynthesisResult)
    # Optional — omit unless a side-by-side comparison genuinely aids understanding.
    table: Optional[ComparisonTable] = None

    def by_id(self) -> Dict[int, AnalysisArticle]:
        return {a.id: a for a in self.articles}


def based_on(level: ContentLevel) -> str:
    return {
        ContentLevel.FULL_TEXT: "full text",
        ContentLevel.ABSTRACT_ONLY: "abstract",
        ContentLevel.TITLE_ONLY: "title only",
    }[level]


def article_key(a: Article) -> str:
    """Stable key used by the renderer to bind a summary to its article."""
    keys = a.dedup_keys()
    return keys[0] if keys else (a.source_id or a.title[:60])
