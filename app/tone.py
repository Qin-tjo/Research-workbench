"""Tone lint — flag AI-tell phrases and hype so output reads like a scientist.

Used as a post-generation check (tests) and to give the analysis step a concrete
ban list (see GROUNDING_RULES / TONE_RULES in app/analysis.py and the skill).
"""

from __future__ import annotations

import re
from typing import List

BANNED_PHRASES = [
    "delve",
    "delves into",
    "it is important to note",
    "it's important to note",
    "it is worth noting",
    "in conclusion",
    "in summary,",
    "groundbreaking",
    "cutting-edge",
    "game-changer",
    "game changer",
    "revolutionary",
    "a testament to",
    "navigating the",
    "in the realm of",
    "in the ever-evolving",
    "plays a crucial role",
    "plays a vital role",
    "this study sheds light",
    "as an ai",
    "i cannot",
    "i'm sorry",
    "let's dive in",
    "unleash",
    "harness the power",
]


def find_tone_violations(text: str) -> List[str]:
    if not text:
        return []
    low = text.lower()
    return [p for p in BANNED_PHRASES if p in low]


def banned_list_for_prompt() -> str:
    return ", ".join(f'"{p}"' for p in BANNED_PHRASES)


_SENTENCE_SPLIT = re.compile(r"(?<=[.!?])\s+")


def split_sentences(text: str) -> List[str]:
    return [s.strip() for s in _SENTENCE_SPLIT.split(text or "") if s.strip()]
