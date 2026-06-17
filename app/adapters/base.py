"""Source adapter contract.

A source = one subclass of `SourceAdapter` registered via `@register`. The
pipeline only knows about this interface, so new sources never touch core code.
"""

from __future__ import annotations

import abc
from enum import Enum
from typing import List

from app.models.article import Article, SearchFilters


class SourceKind(str, Enum):
    API = "api"
    SCRAPE = "scrape"


class SourceAdapter(abc.ABC):
    #: Stable identifier used in the API, source picker, and config.
    name: str = ""
    #: Human-readable label for the UI.
    label: str = ""
    #: Grouping for the source picker UI.
    group: str = "Other"
    kind: SourceKind = SourceKind.API
    #: Whether this source is selected by default in a fresh search.
    default_on: bool = False

    @abc.abstractmethod
    async def search(self, query: str, filters: SearchFilters) -> List[Article]:
        """Return normalized articles matching the query."""
        raise NotImplementedError
