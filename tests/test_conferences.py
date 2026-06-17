"""Conference adapter abstract-detection (no network)."""

from __future__ import annotations

from app.adapters.conferences import AACRAdapter, ASHAdapter, ESMOAdapter


def test_aacr_detects_abstract_titles():
    a = AACRAdapter()
    assert a._looks_like_abstract("Abstract 2817: Establishment of KRAS G12C model")
    assert a._looks_like_abstract("Abstract LB-123: Novel inhibitor")
    assert not a._looks_like_abstract("Targeting KRAS in cancer: a review")


def test_esmo_detects_congress_numbers():
    e = ESMOAdapter()
    assert e._looks_like_abstract("205P Identification of novel bypasses of KRAS")
    assert e._looks_like_abstract("397PD KRAS mutation-induced upregulation")
    assert e._looks_like_abstract("1O First-line therapy results")
    assert not e._looks_like_abstract("A randomized phase III trial of osimertinib")


def test_adapters_registered_in_conferences_group():
    from app.adapters import registry

    names = registry.available_source_names()
    for n in ["asco", "aacr", "esmo", "ash"]:
        assert n in names
    cls = {c.name: c for c in registry.all_adapter_classes()}
    assert cls["aacr"].group == "Conferences"
    assert ASHAdapter.issns  # ISSNs configured
