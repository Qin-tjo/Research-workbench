"""Application configuration loaded from environment / .env.

No API key is required — the LLM analysis is done by the Claude Code session, not a
hosted API. The optional keys below only make the (free) data sources politer/faster.
"""

from __future__ import annotations

from functools import lru_cache
from typing import Optional

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    model_config = SettingsConfigDict(
        env_file=".env", env_file_encoding="utf-8", extra="ignore"
    )

    # Optional source credentials (all free; raise rate limits / politeness pools).
    ncbi_api_key: Optional[str] = None
    ncbi_tool_email: Optional[str] = None
    s2_api_key: Optional[str] = None

    # Storage / behaviour
    cache_dir: str = ".cache"
    default_max_results: int = 20
    request_timeout_s: float = 30.0
    # Report tiering: how many top abstract-bearing papers get deep analysis,
    # and how many of the long tail to list.
    key_paper_count: int = 25
    max_additional_display: int = 100


@lru_cache
def get_settings() -> Settings:
    return Settings()
