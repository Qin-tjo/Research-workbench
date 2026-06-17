"""OncoLit — oncology literature search & synthesis engine.

Package layout:
  adapters/     — one class per data source (PubMed, OpenAlex, EuropePMC, …)
  pipeline/     — search orchestration, dedup, rank, analysis models, tone lint
  report/       — Jinja2 renderer + report.html template
  models/       — shared Article / SearchFilters Pydantic models
  core/         — HTTP client, rate limiter, disk cache, settings
  cli.py        — `python -m app.cli search/render/ask` entrypoint
  target_intel/ — R-based target intelligence dashboard (separate stack, see CLAUDE.md)
"""

__version__ = "0.1.0"
