"""ASCO/ESMO RSS feed parsing + filtering (no network)."""

from __future__ import annotations

from app.adapters.feeds import ASCOFeedAdapter, ESMOFeedAdapter
from app.models.article import ContentLevel, SearchFilters

ASCO_FEED = """<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns="http://purl.org/rss/1.0/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:prism="http://prismstandard.org/namespaces/basic/2.0/">
 <item rdf:about="https://ascopubs.org/doi/abs/10.1200/JCO.2026.44.17_suppl.LBA1">
   <title>Apalutamide for KRAS-mutant prostate cancer: LBA1</title>
   <link>https://ascopubs.org/doi/abs/10.1200/JCO.2026.44.17_suppl.LBA1?af=R</link>
   <description>Journal of Clinical Oncology, Volume 44, Issue 17_suppl</description>
   <dc:identifier>doi:10.1200/JCO.2026.44.17_suppl.LBA1</dc:identifier>
   <prism:doi>10.1200/JCO.2026.44.17_suppl.LBA1</prism:doi>
   <prism:publicationName>Journal of Clinical Oncology</prism:publicationName>
   <dc:date>2026-06-03T07:00:00Z</dc:date>
   <dc:creator>Martin Gleave</dc:creator>
   <dc:creator>Jane Doe</dc:creator>
 </item>
 <item rdf:about="x">
   <title>An unrelated dermatology editorial</title>
   <link>https://ascopubs.org/doi/full/10.1200/JCO.2026.44.10.1000?af=R</link>
   <prism:doi>10.1200/JCO.2026.44.10.1000</prism:doi>
   <dc:date>2026-04-01</dc:date>
 </item>
</rdf:RDF>"""

ESMO_FEED = """<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns="http://purl.org/rss/1.0/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:prism="http://prismstandard.org/namespaces/basic/2.0/">
 <item rdf:about="x">
   <title>205P KRAS G12C inhibitor real-world outcomes</title>
   <link>https://www.annalsofoncology.org/article/S0923-7534(26)00935-X/fulltext?rss=yes</link>
   <dc:date>2026-06-11</dc:date>
   <dc:creator>A B</dc:creator>
 </item>
</rdf:RDF>"""


def test_asco_feed_parses_meeting_abstract_and_strips_query():
    arts = ASCOFeedAdapter()._parse_feed(ASCO_FEED)
    assert len(arts) == 2
    a = arts[0]
    assert a.doi == "10.1200/JCO.2026.44.17_suppl.LBA1"
    assert a.url == "https://ascopubs.org/doi/abs/10.1200/JCO.2026.44.17_suppl.LBA1"  # no ?af=R
    assert a.year == 2026
    assert a.content_level == ContentLevel.TITLE_ONLY
    assert a.is_conference is True  # "suppl" in DOI
    assert a.conference == "ASCO / Journal of Clinical Oncology"
    assert [au.name for au in a.authors] == ["Martin Gleave", "Jane Doe"]
    # The non-supplement article is not flagged as a conference abstract.
    assert arts[1].is_conference is False


def test_esmo_feed_detects_congress_number_and_link_doi_absent():
    arts = ESMOFeedAdapter()._parse_feed(ESMO_FEED)
    assert len(arts) == 1
    a = arts[0]
    assert a.doi is None  # ESMO feed has no prism:doi
    assert a.url.endswith("/fulltext")  # query stripped
    assert a.is_conference is True  # "205P" congress number
    assert a.venue == "Annals of Oncology"


def test_feed_keyword_and_year_filter():
    adapter = ASCOFeedAdapter()
    items = adapter._parse_feed(ASCO_FEED)
    # Keyword "KRAS" matches only the first item.
    hits = adapter._filter(items, "KRAS", SearchFilters(max_results_per_source=10))
    assert len(hits) == 1 and "KRAS" in hits[0].title
    # Year filter excludes everything before 2027.
    none = adapter._filter(items, "", SearchFilters(year_from=2027, max_results_per_source=10))
    assert none == []
