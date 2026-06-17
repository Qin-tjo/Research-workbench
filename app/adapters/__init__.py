"""Importing this package registers all built-in adapters.

Add a new source by creating a module here with an `@register`ed SourceAdapter
subclass and importing it below.
"""

from app.adapters import (  # noqa: F401
    clinicaltrials,
    conferences,
    crossref,
    europepmc,
    feeds,
    openalex,
    openfda,
    preprints,
    pubmed,
    semantic_scholar,
)

__all__ = [
    "pubmed",
    "openalex",
    "europepmc",
    "semantic_scholar",
    "clinicaltrials",
    "openfda",
    "crossref",
    "preprints",
    "conferences",
    "feeds",
]
