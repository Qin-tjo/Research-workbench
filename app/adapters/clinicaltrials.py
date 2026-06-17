"""ClinicalTrials.gov v2 adapter — oncology trials and results.

Distinct content type (interventional/observational studies) highly relevant to
this audience. Trials are mapped to the canonical Article shape with the NCT id
as the identifier and the brief summary as the abstract.
"""

from __future__ import annotations

from typing import List, Optional

from app.adapters.base import SourceAdapter, SourceKind
from app.adapters.registry import register
from app.core.http import DiskCache, TokenBucket, get_json
from app.models.article import Article, ContentLevel, SearchFilters

STUDIES = "https://clinicaltrials.gov/api/v2/studies"


@register
class ClinicalTrialsAdapter(SourceAdapter):
    name = "clinicaltrials"
    label = "ClinicalTrials.gov"
    group = "Trials & regulatory"
    kind = SourceKind.API
    default_on = True

    def __init__(self) -> None:
        self.bucket = TokenBucket(5.0)
        self.cache = DiskCache("clinicaltrials")

    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        params = {
            "query.term": query,
            "pageSize": str(min(filters.max_results_per_source, 100)),
            "format": "json",
        }
        cache_key = str(sorted(params.items()))
        cached = self.cache.get(cache_key)
        if cached is not None:
            studies = cached
        else:
            data = await get_json(STUDIES, params, self.bucket)
            studies = (data or {}).get("studies", [])
            self.cache.set(cache_key, studies)
        return [self._parse(s) for s in studies]

    def _parse(self, study: dict) -> Article:
        ps = study.get("protocolSection", {})
        idm = ps.get("identificationModule", {})
        nct = idm.get("nctId")
        title = idm.get("briefTitle") or idm.get("officialTitle") or "(untitled trial)"
        summary = ps.get("descriptionModule", {}).get("briefSummary")

        status = ps.get("statusModule", {}).get("overallStatus")
        phases = ps.get("designModule", {}).get("phases") or []
        phase = ", ".join(p.replace("PHASE", "Phase ") for p in phases) if phases else None
        venue = "Clinical trial" + (f" ({phase})" if phase else "")

        year = _start_year(ps)
        conditions = ps.get("conditionsModule", {}).get("conditions") or []
        # Fold status + conditions into the summary so the report shows trial context.
        prefix_bits = [b for b in [status, "; ".join(conditions[:4])] if b]
        prefix = (" · ".join(prefix_bits) + "\n\n") if prefix_bits else ""
        abstract = (prefix + summary) if summary else (prefix or None)

        return Article(
            nct_id=nct,
            source_id=nct,
            title=title.strip(),
            venue=venue,
            year=year,
            pub_type="Clinical Trial",
            abstract=abstract,
            content_level=ContentLevel.ABSTRACT_ONLY if abstract else ContentLevel.TITLE_ONLY,
            source=self.name,
            url=f"https://clinicaltrials.gov/study/{nct}" if nct else None,
        )


def _start_year(ps: dict) -> Optional[int]:
    for key in ("startDateStruct", "studyFirstPostDateStruct"):
        date = ps.get("statusModule", {}).get(key, {}).get("date")
        if date and len(date) >= 4 and date[:4].isdigit():
            return int(date[:4])
    return None
