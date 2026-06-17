"""Offline parse tests for Crossref, preprints, Semantic Scholar,
ClinicalTrials.gov, and openFDA."""

from __future__ import annotations

from app.adapters.clinicaltrials import ClinicalTrialsAdapter
from app.adapters.crossref import parse_item
from app.adapters.openfda import OpenFDAAdapter
from app.adapters.preprints import _is_target_server
from app.adapters.semantic_scholar import SemanticScholarAdapter
from app.models.article import ContentLevel

# ---- Crossref ----

CROSSREF_ITEM = {
    "DOI": "10.1/abc",
    "title": ["A KRAS paper"],
    "container-title": ["Nature"],
    "issued": {"date-parts": [[2024, 5, 1]]},
    "author": [{"given": "Jane", "family": "Smith"}],
    "abstract": "<jats:p>Findings here.</jats:p>",
    "is-referenced-by-count": 9,
    "type": "journal-article",
    "URL": "https://doi.org/10.1/abc",
}


def test_crossref_parse_strips_jats_and_maps_fields():
    a = parse_item(CROSSREF_ITEM, "crossref")
    assert a.doi == "10.1/abc"
    assert a.venue == "Nature"
    assert a.year == 2024
    assert a.abstract == "Findings here."
    assert a.citation_count == 9
    assert a.content_level == ContentLevel.ABSTRACT_ONLY
    assert a.authors[0].name == "Jane Smith"


# ---- preprints ----


def test_preprint_server_detection():
    assert _is_target_server({"institution": [{"name": "bioRxiv"}], "DOI": "10.1101/2022.08.19.1"})
    assert _is_target_server({"DOI": "10.1101/2021.01.01.123456"})  # date-pattern fallback
    assert not _is_target_server({"DOI": "10.1101/gad.348523.121"})  # CSHL journal, not preprint
    other = {"institution": [{"name": "Research Square"}], "DOI": "10.21203/x"}
    assert not _is_target_server(other)


# ---- Semantic Scholar ----

S2_ITEM = {
    "paperId": "abc123",
    "title": "KRAS G12C review",
    "abstract": None,
    "tldr": {"text": "A concise TLDR."},
    "year": 2025,
    "venue": "Cell",
    "citationCount": 120,
    "influentialCitationCount": 7,
    "externalIds": {"DOI": "10.2/xy", "PubMed": 12345},
    "authors": [{"name": "A B"}],
    "publicationTypes": ["Review"],
}


def test_s2_parse_uses_tldr_when_no_abstract():
    a = SemanticScholarAdapter()._parse(S2_ITEM)
    assert a.doi == "10.2/xy"
    assert a.pmid == "12345"
    assert a.abstract == "A concise TLDR."
    assert a.content_level == ContentLevel.ABSTRACT_ONLY
    assert a.citation_count == 120
    assert a.venue == "Cell"


# ---- ClinicalTrials.gov ----

CT_STUDY = {
    "protocolSection": {
        "identificationModule": {"nctId": "NCT01234567", "briefTitle": "A KRAS trial"},
        "descriptionModule": {"briefSummary": "Testing a KRAS G12C inhibitor."},
        "statusModule": {"overallStatus": "RECRUITING", "startDateStruct": {"date": "2023-06-01"}},
        "designModule": {"phases": ["PHASE1", "PHASE2"]},
        "conditionsModule": {"conditions": ["NSCLC", "Solid Tumor"]},
    }
}


def test_clinicaltrials_parse():
    a = ClinicalTrialsAdapter()._parse(CT_STUDY)
    assert a.nct_id == "NCT01234567"
    assert a.year == 2023
    assert "Phase 1" in a.venue and "Phase 2" in a.venue
    assert "RECRUITING" in a.abstract
    assert "NSCLC" in a.abstract
    assert a.url == "https://clinicaltrials.gov/study/NCT01234567"
    assert "NCT01234567" in a.dedup_keys()[0]


# ---- openFDA ----

FDA_RESULT = {
    "id": "label-1",
    "effective_time": "20240115",
    "indications_and_usage": ["Indicated for the treatment of metastatic NSCLC."],
    "openfda": {
        "brand_name": ["Tagrisso"],
        "generic_name": ["OSIMERTINIB"],
        "spl_set_id": ["set-123"],
    },
}


def test_openfda_parse():
    a = OpenFDAAdapter()._parse(FDA_RESULT)
    assert "Tagrisso" in a.title
    assert a.year == 2024
    assert "metastatic NSCLC" in a.abstract
    assert a.url == "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=set-123"
    assert a.content_level == ContentLevel.ABSTRACT_ONLY
