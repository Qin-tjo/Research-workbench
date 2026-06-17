---
name: litsearch
description: Oncology/biomedical literature search that produces a clean, science-first HTML report. Searches PubMed, OpenAlex, Europe PMC, Crossref, conference abstracts (ASCO/AACR/ESMO/ASH), ClinicalTrials.gov, openFDA, and bioRxiv/medRxiv, pulls open-access full text where available, then YOU (this Claude session) write the summaries and synthesis — no API key, no server. Trigger when the user wants a literature search, lit review, evidence scan, or to find/summarize papers on a topic (e.g. "/litsearch KRAS G12C resistance", "find recent papers on …", "lit review on …").
---

# litsearch — literature search → science-first HTML report

You are the analysis engine. Python does the key-free work (search, full-text fetch, dedup,
rank, render); **you** read the papers and write the analysis. Run commands from the project
root with the venv interpreter: `.venv/bin/python`.

## Steps

### 1. Get the research question
- If the user already gave a clear question (e.g. `/litsearch KRAS G12C resistance …`), use it.
- **If the question is missing or vague, open the input form** and let them fill it in:
  ```
  .venv/bin/python -m app.cli ask -o /tmp/oncolit_request.json
  ```
  This opens a browser form and blocks until they submit, writing `query`, `years`, `max`,
  `sources`, and `prioritize_citations` to the file. Read it and use those values.

### 2. Run the search (no API key; fetches OA full text for the key papers)
```
.venv/bin/python -m app.cli search "<query>" [--years 2022-2026] [--sources a,b] [--max N] [--no-citation-priority] -o /tmp/oncolit_run.json
```
Defaults: all sources except Semantic Scholar; well-cited papers weighted up. It prints how
many key papers were found and how many got full text. If **0 key papers**, tell the user and stop.

### 3. Read the run file
Read `/tmp/oncolit_run.json` → `key_papers` is a list of `{id, score, article}`. Each
`article` has `title`, `abstract`, and — when open access — `full_text` (a long field) with
`content_level` set to `full_text`. **Analyze the `full_text` when present; otherwise the
`abstract`.** Only analyze `key_papers`; `additional` is listed automatically, don't touch it.

### 4. Write the analysis → `/tmp/oncolit_analysis.json` (Write tool)
```json
{
  "articles": [ { "id": 1, "tldr": "2-4 sentence grounded summary of this paper's science" } ],
  "synthesis": {
    "executive_summary": ["A cited key finding [1][4].", "..."],
    "themes": [ { "heading": "Short theme", "body": "1-2 paragraphs with inline [n] citations." } ]
  },
  "table": null
}
```
Rules:
- **One `articles` entry per key paper** (match `id`). `tldr` conveys the actual science —
  what was studied, in what model/population, and the key result — using the full text when
  available, else the abstract.
- **Grounding — never fabricate.** Use only that paper's retrieved text. Don't state values
  that aren't in it. Quote numbers exactly. (Title-only papers: keep the tldr to what the
  title supports.)
- **synthesis** is the heart of the report: `executive_summary` = 3-5 standalone key findings,
  most important first; `themes` = 2-5 sections grouping related science, noting consensus,
  conflicts, and gaps; say plainly when evidence is thin. **Cite with `[id]`** — every claim
  carries ≥1 citation; the renderer links `[n]` to paper n.
- **table is OPTIONAL — default to `null`.** Include a `ComparisonTable` ONLY when a
  side-by-side comparison genuinely clarifies the science (e.g. comparing drugs × mechanism ×
  outcome across trials). Do **not** add a table of generic metadata. When you do include one:
  ```json
  "table": { "caption": "…", "columns": ["drug","mechanism","key_result"],
             "rows": [ {"id": 1, "cells": {"drug": "…","mechanism": "…","key_result": "…"}} ] }
  ```
  Keep it to columns that carry scientific meaning; use "—" for a genuinely empty cell.
- **No charts/plots.** The report intentionally has none unless they'd support the science
  (not currently generated). Don't ask for stats visualizations.
- **Tone**: plain, declarative scientific prose, like briefing a colleague. No hype, no
  first-person, no hedging boilerplate. Avoid "delve", "groundbreaking", "cutting-edge",
  "it is important to note", "in conclusion", "plays a crucial role", "sheds light", etc.

### 5. Render and open
```
.venv/bin/python -m app.cli render /tmp/oncolit_run.json /tmp/oncolit_analysis.json -o "reports/<slug>-<YYYY-MM-DD>.html"
open "reports/<slug>-<YYYY-MM-DD>.html"
```
Use a short kebab-case `<slug>` from the query.

### 6. Summarize to the user
One short paragraph: papers found / analyzed (and how many had full text), the top 2-3
findings, the report path, and any source warnings. Mention the report is interactive
(keyword filter, source chips).

## Notes
- No `ANTHROPIC_API_KEY` anywhere — the analysis is your output, billed to this session.
- The report is a single self-contained, interactive HTML file. Every fact links to its
  primary source; full-text vs abstract vs title-only is badged per paper.
