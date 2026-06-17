"""ASCO / ESMO latest-issue RSS feed adapters (freshness tier).

ROBOTS OVERRIDE — read this before extending:
ascopubs.org and annalsofoncology.org serve real RSS feeds at `/action/showFeed`,
but their robots.txt has `Disallow: /action` (and `/rss`). These adapters fetch
those feeds anyway, at the user's explicit instruction, for a personal-use
research tool. We keep it polite — low request rate, on-disk caching, an
identifying User-Agent, and no Cloudflare bypass (the feed endpoints are not
behind the challenge; their search/TOC pages are, and we never touch those).

These are *latest-issue* feeds, not a keyword search API: we fetch the current
feed and filter its items by the query client-side. The value is freshness —
surfacing brand-new ASCO Annual Meeting / ESMO Congress abstracts (published as
journal supplements) as soon as they post, ahead of PubMed indexing. The feed
carries titles, authors, links, and DOIs but not abstract bodies, so these
records are title-level; click through for the abstract.
"""

from __future__ import annotations

import asyncio
import re
import xml.etree.ElementTree as ET
from typing import List, Optional

import httpx

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.http import DiskCache, TokenBucket
from app.models.article import Article, Author, ContentLevel, SearchFilters

_NS = {
    "rss": "http://purl.org/rss/1.0/",
    "dc": "http://purl.org/dc/elements/1.1/",
    "prism": "http://prismstandard.org/namespaces/basic/2.0/",
}
_UA = {"User-Agent": "OncoLit/0.1 (research literature tool)"}
_ESMO_NUM = re.compile(r"^\s*\d+[A-Z]{1,3}\b")  # ESMO congress abstract numbers


class _FeedAdapter(SourceAdapter):
    kind = SourceKind.SCRAPE
    group = "Conferences"
    default_on = True

    feed_url: str = ""
    default_venue: str = ""
    conference_label: str = ""

    def __init__(self) -> None:
        self.bucket = TokenBucket(1.0)  # polite: feeds change at most a few times/day
        self.cache = DiskCache("feeds")

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        xml = await self._fetch()
        if not xml:
            return []
        items = self._parse_feed(xml)
        return self._filter(items, query, filters)

    async def _fetch(self, retries: int = 2) -> Optional[str]:
        cache_key = self.feed_url
        cached = self.cache.get(cache_key)
        if cached is not None:
            return cached
        last_exc: Optional[Exception] = None
        for attempt in range(retries + 1):
            await self.bucket.acquire()
            try:
                async with httpx.AsyncClient(timeout=30.0, headers=_UA) as client:
                    resp = await client.get(self.feed_url, follow_redirects=True)
                    resp.raise_for_status()
                    self.cache.set(cache_key, resp.text)
                    return resp.text
            except (httpx.TimeoutException, httpx.HTTPStatusError) as exc:
                last_exc = exc
                if isinstance(exc, httpx.HTTPStatusError) and exc.response.status_code < 500:
                    raise
                await asyncio.sleep(1.5 * (attempt + 1))
        raise last_exc  # type: ignore[misc]

    def _parse_feed(self, xml: str) -> List[Article]:
        try:
            root = ET.fromstring(xml)
        except ET.ParseError:
            return []
        return [self._parse_item(it) for it in root.findall("rss:item", _NS)]

    def _parse_item(self, it: ET.Element) -> Article:
        title = _text(it, "rss:title") or "(untitled)"
        link = _strip_query(_text(it, "rss:link"))
        doi = _text(it, "prism:doi") or _strip_doi_prefix(_text(it, "dc:identifier"))
        venue = _text(it, "prism:publicationName") or self.default_venue
        year = _year(_text(it, "dc:date"))
        authors = [
            Author(name=e.text.strip())
            for e in it.findall("dc:creator", _NS)
            if e.text and e.text.strip()
        ]
        is_conf = self._is_conference(title, link, doi)
        return Article(
            doi=doi,
            source_id=doi or link,
            title=title.strip(),
            authors=authors,
            venue=venue,
            year=year,
            is_conference=is_conf,
            conference=self.conference_label if is_conf else None,
            pub_type="Meeting Abstract" if is_conf else None,
            content_level=ContentLevel.TITLE_ONLY,  # feeds carry no abstract body
            source=self.name,
            url=link or (f"https://doi.org/{doi}" if doi else None),
        )

    def _is_conference(self, title: str, link: Optional[str], doi: Optional[str]) -> bool:
        blob = f"{link or ''} {doi or ''}".lower()
        if "suppl" in blob:  # ASCO/ESMO meeting abstracts publish in journal supplements
            return True
        return bool(_ESMO_NUM.search(title or ""))

    def _filter(
        self, items: List[Article], query: str, filters: SearchFilters
    ) -> List[Article]:
        tokens = {t for t in re.findall(r"[a-z0-9]+", query.lower()) if len(t) > 2}
        out: List[Article] = []
        for art in items:
            if filters.year_from and art.year and art.year < filters.year_from:
                continue
            if filters.year_to and art.year and art.year > filters.year_to:
                continue
            if tokens:
                hay = f"{art.title} {' '.join(a.name for a in art.authors)}".lower()
                if not any(t in hay for t in tokens):
                    continue
            out.append(art)
            if len(out) >= filters.max_results_per_source:
                break
        return out


@register
class ASCOFeedAdapter(_FeedAdapter):
    name = "asco_feed"
    label = "ASCO (latest feed)"
    feed_url = "https://ascopubs.org/action/showFeed?type=etoc&feed=rss&jc=jco"
    default_venue = "Journal of Clinical Oncology"
    conference_label = "ASCO / Journal of Clinical Oncology"


@register
class ESMOFeedAdapter(_FeedAdapter):
    name = "esmo_feed"
    label = "ESMO (latest feed)"
    feed_url = "https://www.annalsofoncology.org/action/showFeed?type=etoc&feed=rss&jc=annonc"
    default_venue = "Annals of Oncology"
    conference_label = "ESMO / Annals of Oncology"


def _text(el: ET.Element, path: str) -> Optional[str]:
    found = el.find(path, _NS)
    if found is None:
        return None
    return "".join(found.itertext()).strip() or None


def _strip_query(url: Optional[str]) -> Optional[str]:
    if not url:
        return None
    return url.split("?", 1)[0]


def _strip_doi_prefix(value: Optional[str]) -> Optional[str]:
    if not value:
        return None
    return value[4:] if value.lower().startswith("doi:") else value


def _year(date: Optional[str]) -> Optional[int]:
    if date and len(date) >= 4 and date[:4].isdigit():
        return int(date[:4])
    return None
