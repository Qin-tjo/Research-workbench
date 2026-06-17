# OncoLit

Oncology literature search & summary engine — a **Claude Code skill**. Describe what you
want to find; OncoLit searches biomedical sources, de-duplicates and ranks them, and
produces a **self-contained, interactive HTML report** with a cited synthesis, a
comparison table, per-paper cards, and charts.

**No API key, no server.** Python does the key-free work (search, dedup, rank, render);
the analysis (per-paper TL;DRs, the query-specific table, the thematic synthesis) is
written by your Claude Code session. Nothing calls a hosted LLM API.

## Use it

In Claude Code, from this project:

```
/litsearch KRAS G12C inhibitor resistance in NSCLC, 2023-2025
```

Claude runs the search, writes the analysis, renders the report, and opens it. You can also
just ask in plain language ("find recent papers on …", "lit review on …"). If you invoke it
without a clear question, it opens a small **HTML input form** in your browser (research
question, year range, sources, citation weighting); fill it in and it generates the report.

## How it works

```
/litsearch
  → [optional] python -m app.cli ask -o request.json     (ephemeral HTML form for the question)
  → python -m app.cli search "<query>" … -o run.json     (14 sources → dedup → rank → tier → OA full text)
  → Claude session reads run.json, writes analysis.json   (TL;DRs + synthesis, optional table — no API)
  → python -m app.cli render run.json analysis.json -o reports/<name>.html
  → open the report
```

The report is **science-first**: key findings, a thematic synthesis, and per-paper cards.
There are no generic "paper stats" charts; a comparison table appears only when the analysis
decides it supports the science. Well-cited papers are weighted up in selection (toggleable).
For the ~25 key papers, open-access **full text** is fetched (Europe PMC/PMC) and analyzed
when available; paywalled papers fall back to the abstract.

- **Sources** (`app/adapters/`): a plugin registry of 14 key-free sources —
  *Indexed* (PubMed, OpenAlex, Europe PMC, Semantic Scholar, Crossref), *Conferences*
  (ASCO, AACR, ESMO, ASH + ASCO/ESMO RSS feeds), *Trials & regulatory* (ClinicalTrials.gov,
  openFDA), *Preprints* (bioRxiv/medRxiv). All on by default except Semantic Scholar (needs
  `S2_API_KEY`). Add a source by adding one class.
- **Pipeline** (`app/pipeline/`): dedup by DOI/PMID/fuzzy title, transparent composite
  ranking, a uniform year-range guard, and tiering into ~25 abstract-bearing **key papers**
  (deep-analyzed) vs. an **additional** long tail (listed, not analyzed).
- **Analysis contract** (`app/analysis.py` + `.claude/skills/litsearch/SKILL.md`): the
  session writes `analysis.json` (validated by the `Analysis` model) following strict
  grounding ("only the abstract; else 'Not reported'") and tone (no AI-tells) rules.
- **Report** (`app/report/`): Jinja2 + inline SVG charts, fully self-contained and
  interactive — keyword filter, source chips, sortable table, sticky section nav. Every
  fact links to its primary source; abstract-only/title-only papers are badged.

## CLI (used by the skill, runnable directly)

```bash
python -m app.cli search "KRAS G12C resistance NSCLC" --years 2023-2025 --max 20 -o run.json
# ... write analysis.json (the skill tells Claude how) ...
python -m app.cli render run.json analysis.json -o reports/report.html
```

`--sources a,b` limits sources; `--no-preprints` excludes preprints.

## Setup

```bash
python3 -m venv .venv
.venv/bin/python -m pip install -U pip
.venv/bin/python -m pip install -e ".[dev]"
```

No `.env` is required. Optional keys (all free, source-politeness only): `NCBI_API_KEY`,
`NCBI_TOOL_EMAIL` (faster PubMed/OpenAlex/Crossref), `S2_API_KEY` (enables Semantic Scholar).

## Tests

```bash
.venv/bin/pytest        # offline: adapters, pipeline, report, CLI render
.venv/bin/ruff check .
```

## Notes & limits

- Conference search pages are robots-disallowed + Cloudflare-walled, so ASCO/AACR/ESMO/ASH
  are reached via Crossref by journal ISSN; ASCO/ESMO additionally via their RSS feeds (a
  documented robots override, kept polite). Feed records are title-level.
- Python 3.9 compatible (uses `typing.Optional/List`, stdlib `difflib`).
- Out of scope: multi-user, deployment, scheduled runs.
