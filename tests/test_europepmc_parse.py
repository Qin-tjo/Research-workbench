"""Europe PMC result parsing (no network)."""

from __future__ import annotations

from app.adapters.europepmc import EuropePMCAdapter
from app.models.article import ContentLevel, SearchFilters

JOURNAL_RESULT = {
    "id": "40000001",
    "source": "MED",
    "pmid": "40000001",
    "doi": "10.1/jco.1",
    "title": "Osimertinib resistance in EGFR-mutant NSCLC",
    "authorString": "Smith J, Doe A, Lee K",
    "pubYear": "2024",
    "citedByCount": 31,
    "abstractText": "Resistance mechanisms were profiled.",
    "journalInfo": {"journal": {"title": "Journal of Clinical Oncology"}},
    "pubTypeList": {"pubType": ["Journal Article"]},
}

PREPRINT_RESULT = {
    "id": "PPR123456",
    "source": "PPR",
    "doi": "10.1101/2024.01.01",
    "title": "A preprint on KRAS inhibitors",
    "authorString": "Roe B",
    "pubYear": "2024",
    "citedByCount": 0,
    "pubTypeList": {"pubType": ["Preprint"]},
}


def test_parse_journal_article():
    a = EuropePMCAdapter()._parse(JOURNAL_RESULT)
    assert a.pmid == "40000001"
    assert a.doi == "10.1/jco.1"
    assert a.year == 2024
    assert a.venue == "Journal of Clinical Oncology"
    assert a.citation_count == 31
    assert a.content_level == ContentLevel.ABSTRACT_ONLY
    assert a.is_preprint is False
    assert [au.name for au in a.authors] == ["Smith J", "Doe A", "Lee K"]
    assert a.url == "https://europepmc.org/article/MED/40000001"


def test_parse_preprint_without_abstract():
    a = EuropePMCAdapter()._parse(PREPRINT_RESULT)
    assert a.is_preprint is True
    assert a.abstract is None
    assert a.content_level == ContentLevel.TITLE_ONLY
    assert a.venue == "Preprint"


def test_query_builder_applies_year_and_preprint_filters():
    a = EuropePMCAdapter()
    q = a._build_query("KRAS", SearchFilters(year_from=2023, year_to=2026, include_preprints=False))
    assert "(KRAS)" in q
    assert "PUB_YEAR:[2023 TO 2026]" in q
    assert "NOT (SRC:PPR)" in q
