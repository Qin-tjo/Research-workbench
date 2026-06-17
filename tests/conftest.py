"""Test fixtures: isolate the DB and cache from the developer's real files."""

from __future__ import annotations

import pytest


@pytest.fixture(autouse=True)
def _isolate_env(tmp_path, monkeypatch):
    monkeypatch.setenv("DATABASE_URL", f"sqlite:///{tmp_path/'test.db'}")
    monkeypatch.setenv("CACHE_DIR", str(tmp_path / "cache"))
    # Ensure no real key leaks into tests that assert the no-LLM fallback path.
    monkeypatch.delenv("ANTHROPIC_API_KEY", raising=False)
    # Settings are cached via lru_cache; clear so the env overrides take effect.
    from app.core.config import get_settings

    get_settings.cache_clear()
    yield
    get_settings.cache_clear()
