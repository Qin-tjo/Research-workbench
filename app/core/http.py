"""Shared async HTTP client, rate limiting, and a tiny on-disk response cache.

Adapters use this so every outbound call is rate-limited per host and cached,
keeping us polite to free APIs (PubMed etc.) and fast on repeat queries.
"""

from __future__ import annotations

import asyncio
import hashlib
import json
import time
from pathlib import Path
from typing import Any, Dict, Optional

import httpx

from app.core.config import get_settings


class TokenBucket:
    """Simple async token-bucket rate limiter (requests per second)."""

    def __init__(self, rate_per_sec: float) -> None:
        self.rate = rate_per_sec
        self.capacity = max(1.0, rate_per_sec)
        self.tokens = self.capacity
        self.updated = time.monotonic()
        # Created lazily on first acquire so the lock binds to the *running* event loop,
        # not whatever loop existed at construction (module-level buckets are built at
        # import time, before any loop is running).
        self._lock: Optional[asyncio.Lock] = None

    async def acquire(self) -> None:
        if self._lock is None:
            self._lock = asyncio.Lock()
        async with self._lock:
            while True:
                now = time.monotonic()
                self.tokens = min(
                    self.capacity, self.tokens + (now - self.updated) * self.rate
                )
                self.updated = now
                if self.tokens >= 1.0:
                    self.tokens -= 1.0
                    return
                await asyncio.sleep((1.0 - self.tokens) / self.rate)


class DiskCache:
    """Content-addressed JSON cache on disk. Best-effort; failures are ignored."""

    def __init__(self, namespace: str) -> None:
        root = Path(get_settings().cache_dir) / namespace
        root.mkdir(parents=True, exist_ok=True)
        self.root = root

    def _path(self, key: str) -> Path:
        digest = hashlib.sha256(key.encode("utf-8")).hexdigest()
        return self.root / f"{digest}.json"

    def get(self, key: str) -> Optional[Any]:
        path = self._path(key)
        if not path.exists():
            return None
        try:
            return json.loads(path.read_text("utf-8"))
        except (OSError, json.JSONDecodeError):
            return None

    def set(self, key: str, value: Any) -> None:
        try:
            self._path(key).write_text(json.dumps(value), "utf-8")
        except (OSError, TypeError):
            pass


def new_client() -> httpx.AsyncClient:
    settings = get_settings()
    headers = {"User-Agent": "OncoLit/0.1 (research literature tool)"}
    return httpx.AsyncClient(timeout=settings.request_timeout_s, headers=headers)


async def get_json(
    url: str,
    params: dict,
    bucket: "TokenBucket",
    *,
    headers: Optional[Dict[str, str]] = None,
    timeout: float = 45.0,
    retries: int = 2,
) -> Optional[Any]:
    """Rate-limited GET returning parsed JSON, with backoff retry on timeouts/5xx.

    Returns None on a 404 (treated as "no results" by several public APIs, e.g.
    openFDA). 4xx other than 404 raise. Shared by the API adapters.
    """
    base_headers = {"User-Agent": "OncoLit/0.1 (research literature tool)"}
    if headers:
        base_headers.update(headers)
    last_exc: Optional[Exception] = None
    for attempt in range(retries + 1):
        await bucket.acquire()
        try:
            async with httpx.AsyncClient(timeout=timeout, headers=base_headers) as client:
                resp = await client.get(url, params=params)
                if resp.status_code == 404:
                    return None
                resp.raise_for_status()
                return resp.json()
        except (httpx.TimeoutException, httpx.HTTPStatusError) as exc:
            last_exc = exc
            if isinstance(exc, httpx.HTTPStatusError):
                code = exc.response.status_code
                # 429 (rate limited) and 5xx are retryable; other 4xx are not.
                if code != 429 and code < 500:
                    raise
                delay = _retry_after(exc.response) or 1.5 * (attempt + 1)
            else:
                delay = 1.5 * (attempt + 1)
            await asyncio.sleep(delay)
    raise last_exc  # type: ignore[misc]


def _retry_after(response: "httpx.Response") -> Optional[float]:
    raw = response.headers.get("retry-after")
    if not raw:
        return None
    try:
        return min(float(raw), 10.0)  # cap so we don't stall the whole search
    except ValueError:
        return None
