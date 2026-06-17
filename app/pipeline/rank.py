"""Composite ranking.

Score = weighted blend of query relevance (lexical overlap), recency, citation
impact, and a source/venue prior. Kept transparent: each component is recorded
in `score_breakdown` so the UI can explain ordering. (Semantic rerank over a
local vector index is a later enhancement; this is the dependency-free baseline.)
"""

from __future__ import annotations

import datetime
import math
import re
from typing import List

from app.models.article import Article, RankedArticle

# Default blend, and a citation-forward blend used when the user asks to prioritize
# well-cited papers (the input form defaults this on).
_WEIGHTS = {"relevance": 0.45, "recency": 0.2, "citations": 0.25, "source": 0.1}
_WEIGHTS_CITED = {"relevance": 0.4, "recency": 0.1, "citations": 0.45, "source": 0.05}
_CURRENT_YEAR = datetime.date.today().year

# Per-source priors (peer-reviewed indexed literature ranked above metadata-only
# or non-peer-reviewed sources). Unknown sources fall back to 0.6.
_SOURCE_PRIOR = {
    "pubmed": 0.9,
    "europepmc": 0.85,
    "openalex": 0.8,
    "semanticscholar": 0.8,
    "clinicaltrials": 0.7,
    "openfda": 0.7,
    "crossref": 0.65,
    "preprints": 0.6,
    "asco": 0.85,
    "aacr": 0.85,
    "esmo": 0.85,
    "ash": 0.85,
    "asco_feed": 0.85,
    "esmo_feed": 0.85,
}


def _tokenize(text: str) -> set:
    return set(re.findall(r"[a-z0-9]+", text.lower()))


def _relevance(query_tokens: set, art: Article) -> float:
    if not query_tokens:
        return 0.0
    text = f"{art.title} {art.abstract or ''}"
    hits = query_tokens & _tokenize(text)
    return len(hits) / len(query_tokens)


def _recency(art: Article) -> float:
    if not art.year:
        return 0.3
    age = max(0, _CURRENT_YEAR - art.year)
    return math.exp(-age / 6.0)  # ~half-life of a few years


def _citations(art: Article) -> float:
    if not art.citation_count:
        return 0.0
    return min(1.0, math.log10(art.citation_count + 1) / 3.0)  # 1000 cites -> 1.0


def _source_prior(art: Article) -> float:
    base = _SOURCE_PRIOR.get(art.source, 0.6)
    if art.is_conference:
        base = min(1.0, base + 0.05)
    if art.is_preprint:
        base = max(0.0, base - 0.1)
    return base


def rank(
    query: str, articles: List[Article], prioritize_citations: bool = False
) -> List[RankedArticle]:
    weights = _WEIGHTS_CITED if prioritize_citations else _WEIGHTS
    query_tokens = _tokenize(query)
    ranked: List[RankedArticle] = []
    for art in articles:
        components = {
            "relevance": _relevance(query_tokens, art),
            "recency": _recency(art),
            "citations": _citations(art),
            "source": _source_prior(art),
        }
        score = sum(weights[k] * v for k, v in components.items())
        ranked.append(
            RankedArticle(article=art, score=round(score, 4), score_breakdown=components)
        )
    ranked.sort(key=lambda r: r.score, reverse=True)
    return ranked
