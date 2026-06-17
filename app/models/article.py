"""Canonical data models shared across the pipeline.

Every source adapter normalizes its raw payload into an `Article`. Downstream
stages (dedup, ranking, summarization, reporting) only ever see `Article`s, so
adding a new source never ripples past the adapter layer.
"""

from __future__ import annotations

from enum import Enum
from typing import List, Optional

from pydantic import BaseModel, Field


class ContentLevel(str, Enum):
    """How much text we actually have for an article.

    Drives the no-fabrication guardrails: the summarizer is only allowed to make
    claims supported by the text at this level.
    """

    FULL_TEXT = "full_text"
    ABSTRACT_ONLY = "abstract_only"
    TITLE_ONLY = "title_only"


class Author(BaseModel):
    name: str
    affiliation: Optional[str] = None


class Article(BaseModel):
    """A normalized record from any source."""

    # Identity / dedup keys
    doi: Optional[str] = None
    pmid: Optional[str] = None
    nct_id: Optional[str] = None  # ClinicalTrials.gov
    source_id: Optional[str] = None  # raw id within the originating source

    # Bibliographic
    title: str
    authors: List[Author] = Field(default_factory=list)
    venue: Optional[str] = None  # journal or conference name
    conference: Optional[str] = None  # e.g. "ASCO Annual Meeting 2026"
    year: Optional[int] = None
    pub_type: Optional[str] = None  # e.g. "Journal Article", "Meeting Abstract"
    is_conference: bool = False
    is_preprint: bool = False

    # Content
    abstract: Optional[str] = None
    full_text: Optional[str] = None
    content_level: ContentLevel = ContentLevel.TITLE_ONLY

    # Provenance
    source: str = ""  # adapter name, e.g. "pubmed"
    url: Optional[str] = None  # primary, clickable source link

    # Signals
    citation_count: Optional[int] = None

    def best_text(self) -> str:
        """The richest text available, for summarization input."""
        if self.full_text:
            return self.full_text
        if self.abstract:
            return self.abstract
        return self.title

    def dedup_keys(self) -> List[str]:
        """Strong identity keys, most-authoritative first."""
        keys: List[str] = []
        if self.doi:
            keys.append(f"doi:{self.doi.lower()}")
        if self.pmid:
            keys.append(f"pmid:{self.pmid}")
        if self.nct_id:
            keys.append(f"nct:{self.nct_id.upper()}")
        return keys


class SearchFilters(BaseModel):
    """User-controllable constraints applied across adapters."""

    sources: List[str] = Field(default_factory=list)  # adapter names to run
    year_from: Optional[int] = None
    year_to: Optional[int] = None
    max_results_per_source: int = 40
    include_preprints: bool = True
    prioritize_citations: bool = True  # weight well-cited papers more in ranking


class RankedArticle(BaseModel):
    """An article plus its computed ranking score and breakdown."""

    article: Article
    score: float = 0.0
    score_breakdown: dict = Field(default_factory=dict)
