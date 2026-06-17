"""Cross-source de-duplication.

Merge order: strong keys (DOI, PMID, NCT) first, then a fuzzy title+year match
for records that lack shared identifiers. When merging, keep the richest record
(prefer full text > abstract > title) and fill gaps from the duplicate.
"""

from __future__ import annotations

import re
from difflib import SequenceMatcher
from typing import Dict, List

from app.models.article import Article, ContentLevel

_LEVEL_RANK = {
    ContentLevel.FULL_TEXT: 3,
    ContentLevel.ABSTRACT_ONLY: 2,
    ContentLevel.TITLE_ONLY: 1,
}

_TITLE_SIM_THRESHOLD = 92


def _norm_title(title: str) -> str:
    # Lowercase, strip punctuation, and sort tokens so word-order differences match.
    cleaned = re.sub(r"[^a-z0-9 ]", "", title.lower())
    return " ".join(sorted(cleaned.split()))


def _title_similarity(a: str, b: str) -> float:
    """Token-sorted similarity in [0, 100], stdlib-only."""
    return SequenceMatcher(None, a, b).ratio() * 100


def _richness(a: Article) -> int:
    return _LEVEL_RANK.get(a.content_level, 0)


def _merge(primary: Article, other: Article) -> Article:
    """Return primary enriched with any fields it's missing from other."""
    if _richness(other) > _richness(primary):
        primary, other = other, primary
    data = primary.model_dump()
    for field, value in other.model_dump().items():
        if data.get(field) in (None, "", [], 0) and value not in (None, "", [], 0):
            data[field] = value
    # Prefer the higher citation count if both present.
    if other.citation_count and (primary.citation_count or 0) < other.citation_count:
        data["citation_count"] = other.citation_count
    return Article(**data)


def dedup(articles: List[Article]) -> List[Article]:
    by_key: Dict[str, Article] = {}
    keyless: List[Article] = []

    for art in articles:
        keys = art.dedup_keys()
        if not keys:
            keyless.append(art)
            continue
        existing_key = next((k for k in keys if k in by_key), None)
        if existing_key:
            by_key[existing_key] = _merge(by_key[existing_key], art)
        else:
            by_key[keys[0]] = art

    merged = list(by_key.values())

    # Fuzzy title+year pass for records without shared identifiers.
    for art in keyless:
        match = _find_fuzzy_match(art, merged)
        if match is not None:
            idx = merged.index(match)
            merged[idx] = _merge(match, art)
        else:
            merged.append(art)

    return merged


def _find_fuzzy_match(art: Article, pool: List[Article]):
    nt = _norm_title(art.title)
    for candidate in pool:
        if art.year and candidate.year and art.year != candidate.year:
            continue
        if _title_similarity(nt, _norm_title(candidate.title)) >= _TITLE_SIM_THRESHOLD:
            return candidate
    return None
