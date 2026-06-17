"""PubMed adapter via NCBI E-utilities (esearch -> efetch).

Abstracts are available for nearly all records, so this is the backbone source
and always at least abstract-level. Honors NCBI rate limits (3 req/s, or 10 with
an API key).
"""

from __future__ import annotations

import xml.etree.ElementTree as ET
from typing import List, Optional

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.config import get_settings
from app.core.http import DiskCache, TokenBucket, new_client
from app.models.article import Article, Author, ContentLevel, SearchFilters

ESEARCH = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
EFETCH = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"


@register
class PubMedAdapter(SourceAdapter):
    name = "pubmed"
    label = "PubMed / PMC"
    group = "Indexed literature"
    kind = SourceKind.API
    default_on = True

    def __init__(self) -> None:
        settings = get_settings()
        self.api_key = settings.ncbi_api_key
        self.email = settings.ncbi_tool_email
        # 10 req/s with a key, 3 without (we stay just under).
        self.bucket = TokenBucket(9.0 if self.api_key else 2.5)
        self.cache = DiskCache("pubmed")

    def _common_params(self) -> dict:
        params = {"db": "pubmed", "tool": "oncolit"}
        if self.api_key:
            params["api_key"] = self.api_key
        if self.email:
            params["email"] = self.email
        return params

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        pmids = await self._esearch(query, filters)
        if not pmids:
            return []
        return await self._efetch(pmids)

    async def _esearch(self, query: str, filters: SearchFilters) -> List[str]:
        params = self._common_params()
        params.update(
            {
                "term": query,
                "retmax": str(filters.max_results_per_source),
                "retmode": "json",
                "sort": "relevance",
            }
        )
        if filters.year_from or filters.year_to:
            params["datetype"] = "pdat"
            params["mindate"] = str(filters.year_from or 1900)
            params["maxdate"] = str(filters.year_to or 3000)

        cache_key = "esearch:" + str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            return cached

        await self.bucket.acquire()
        async with new_client() as client:
            resp = await client.get(ESEARCH, params=params)
            resp.raise_for_status()
            data = resp.json()
        ids = data.get("esearchresult", {}).get("idlist", [])
        self.cache.set(cache_key, ids)
        return ids

    async def _efetch(self, pmids: List[str]) -> List[Article]:
        params = self._common_params()
        params.update({"id": ",".join(pmids), "retmode": "xml"})

        cache_key = "efetch:" + ",".join(pmids)
        cached = self.cache.get(cache_key)
        if cached is not None:
            xml = cached
        else:
            await self.bucket.acquire()
            async with new_client() as client:
                resp = await client.get(EFETCH, params=params)
                resp.raise_for_status()
                xml = resp.text
            self.cache.set(cache_key, xml)

        return self._parse(xml)

    def _parse(self, xml: str) -> List[Article]:
        articles: List[Article] = []
        try:
            root = ET.fromstring(xml)
        except ET.ParseError:
            return articles

        for art in root.findall(".//PubmedArticle"):
            articles.append(self._parse_one(art))
        return articles

    def _parse_one(self, art: ET.Element) -> Article:
        pmid = _text(art, ".//PMID")
        title = _text(art, ".//ArticleTitle") or "(untitled)"
        abstract = _join_abstract(art)
        journal = _text(art, ".//Journal/Title")
        year = _to_int(_text(art, ".//JournalIssue/PubDate/Year"))
        if year is None:
            # MedlineDate fallback like "2025 Jun-Jul"
            md = _text(art, ".//JournalIssue/PubDate/MedlineDate")
            if md:
                year = _to_int(md[:4])
        pub_type = _text(art, ".//PublicationTypeList/PublicationType")
        doi = _doi(art)

        authors: List[Author] = []
        for a in art.findall(".//AuthorList/Author"):
            last = _text(a, "LastName")
            fore = _text(a, "ForeName")
            if last:
                authors.append(Author(name=" ".join(p for p in [fore, last] if p)))

        is_conf = bool(pub_type and "abstract" in pub_type.lower())

        return Article(
            pmid=pmid,
            doi=doi,
            source_id=pmid,
            title=title.strip(),
            authors=authors,
            venue=journal,
            year=year,
            pub_type=pub_type,
            is_conference=is_conf,
            abstract=abstract,
            content_level=(
                ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY
            ),
            source=self.name,
            url=f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/" if pmid else None,
        )


def _text(el: ET.Element, path: str) -> Optional[str]:
    found = el.find(path)
    if found is None:
        return None
    return "".join(found.itertext()).strip() or None


def _join_abstract(art: ET.Element) -> Optional[str]:
    parts = []
    for ab in art.findall(".//Abstract/AbstractText"):
        label = ab.get("Label")
        text = "".join(ab.itertext()).strip()
        if not text:
            continue
        parts.append(f"{label}: {text}" if label else text)
    return "\n".join(parts) if parts else None


def _doi(art: ET.Element) -> Optional[str]:
    for eid in art.findall(".//ArticleIdList/ArticleId"):
        if eid.get("IdType") == "doi":
            return (eid.text or "").strip() or None
    return None


def _to_int(value: Optional[str]) -> Optional[int]:
    if not value:
        return None
    try:
        return int(value)
    except ValueError:
        return None
