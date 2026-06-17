"""Adapter registry — the plugin system for sources.

Importing `app.adapters` (see __init__.py) imports each adapter module, whose
`@register` decorator adds it here. Nothing else needs editing to add a source.
"""

from __future__ import annotations

from typing import Dict, List, Type

from app.adapters.base import SourceAdapter

_REGISTRY: Dict[str, Type[SourceAdapter]] = {}


def register(cls: Type[SourceAdapter]) -> Type[SourceAdapter]:
    if not cls.name:
        raise ValueError(f"{cls.__name__} must set a non-empty `name`")
    if cls.name in _REGISTRY:
        raise ValueError(f"Duplicate adapter name: {cls.name}")
    _REGISTRY[cls.name] = cls
    return cls


def available_source_names() -> List[str]:
    return sorted(_REGISTRY)


def all_adapter_classes() -> List[Type[SourceAdapter]]:
    return list(_REGISTRY.values())


def default_source_names() -> List[str]:
    return sorted(name for name, cls in _REGISTRY.items() if cls.default_on)


def get_adapters(names: List[str]) -> List[SourceAdapter]:
    """Instantiate adapters for the given names (unknown names are skipped)."""
    return [_REGISTRY[n]() for n in names if n in _REGISTRY]
