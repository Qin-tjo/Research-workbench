# Research Workbench

A personal research workbench for producing oncology research reports and dashboards.
Two tools live here side-by-side — they share the same repo but have independent stacks.

---

## Tool 1 — OncoLit (Python)

Literature search + synthesis engine, exposed as the `/litsearch` Claude Code skill.

**Trigger:** `/litsearch <query>` or plain "find recent papers on …" / "lit review on …"

**Pipeline:**
```
search (14 key-free sources) → dedup → rank → tier
  → Claude session writes analysis.json (TL;DRs + synthesis, no external API)
  → render → reports/<name>.html (self-contained interactive HTML)
```

**Key directories:**
- `app/adapters/` — one class per source (PubMed, OpenAlex, EuropePMC, Crossref,
  Semantic Scholar, ASCO/AACR/ESMO/ASH, ClinicalTrials.gov, openFDA, bioRxiv/medRxiv)
- `app/pipeline/` — dedup, rank, fulltext fetch, search orchestration
- `app/report/` — Jinja2 renderer + `templates/report.html`
- `app/analysis.py` — `Analysis` Pydantic model (the contract Claude writes to)
- `app/core/` — HTTP client, config (pydantic-settings, reads `.env`)
- `app/cli.py` — `python -m app.cli search/render/ask`
- `.claude/skills/litsearch/SKILL.md` — skill instructions for Claude
- `tests/` — offline unit tests; run with `.venv/bin/pytest`

**Constraints:**
- Python 3.9 compatible — use `typing.Optional`/`typing.List`, not `X | None` or `list[X]`
- No external LLM API calls from Python — analysis is always written by the Claude session
- Sources are key-free by default; optional keys in `.env` (see `.env.example`)
- Conference sites (ASCO/AACR/ESMO/ASH) are robots-disallowed — reached via Crossref ISSN
  + RSS feeds only (documented robots override, polite)

**Setup:**
```bash
python3 -m venv .venv && .venv/bin/python -m pip install -e ".[dev]"
```

---

## Tool 2 — Target Intelligence (R)

Generates a gene-parameterized, single-page target intelligence dashboard from TCGA,
recount3, MC3, ClinicalTrials.gov, and MSK-IMPACT data. Currently built around MTAP/PRMT5.

**To regenerate the dashboard:**
```bash
Rscript app/target_intel/R/12_render_dashboard.R
# Output: app/target_intel/results/target_intel_dashboard.html
# Public copy: app/target_intel/public/index.html (manually cp after render)
```

**Key directories:**
- `app/target_intel/R/` — numbered pipeline scripts (00–12) + utils/
- `app/target_intel/data/` — YAML source-of-truth files for all scientific content:
  - `citations.yaml` — 36 primary citations with stable URLs
  - `panel2_mechanism.yaml` — MTAP/PRMT5 mechanism flow + uncertainties
  - `panel3_drugs.yaml` / `panel3_results.yaml` — clinical trial entries + ORRs
  - `panel4_synthesis.yaml` — thesis, patient selection, combinations, risks
- `app/target_intel/results/` — rendered HTML + intermediate parquets (not committed)
- `app/target_intel/public/` — `index.html` copy for GitHub Pages (not committed here;
  lives in the separate public repo `github.com/Qin-tjo/mtap-intel`)
- `app/target_intel/cache/` — 1 GB raw TCGA/recount3 data; gitignored, regenerable

**Scientific conventions (must follow):**
- Copy number: ABSOLUTE algorithm (Taylor 2018 PanCanAtlas), purity/ploidy-corrected, hg19
- RNA expression: log2(TPM+1) throughout
- All 33 TCGA indications included (never subset silently)
- Tone: hedged — "appears to", "likely", "is thought to" — never bare assertions
- Every claim needs a citation key traceable to `citations.yaml`
- Avoid contrastive phrasings ("rather than", "not X but Y")

**R package notes:**
- `library(SummarizedExperiment)` masks `fs::path` — always qualify as `fs::path()`
- Glue templates use `.open="{"`, `.close="}"` to avoid conflict with CSS `{}`
- `sprintf` CSS rules: double `%%` to escape literal `%`

**Public dashboard:** https://qin-tjo.github.io/mtap-intel/
Snapshot date: 2026-06-17. To publish an update: re-render, cp to `public/index.html`,
push to `github.com/Qin-tjo/mtap-intel`.

---

## Repo hygiene

- `app/target_intel/cache/` — gitignored (large raw data, regenerable)
- `reports/` — gitignored (generated OncoLit HTML reports)
- `oncolit.db` — gitignored (regenerable SQLite cache)
- `.env` — gitignored; copy from `.env.example` (all keys optional)
- `app/target_intel/public/` — gitignored here; managed in the separate `mtap-intel` repo
