# Target Intelligence — R pipeline

## Entry point

```bash
Rscript app/target_intel/R/12_render_dashboard.R
# Output: app/target_intel/results/target_intel_dashboard.html
```

## Script inventory

| Script | Purpose |
|---|---|
| `00_config.R` | Gene symbol, paths, shared constants — sourced by all scripts |
| `01_fetch_tcga.R` | One-time TCGA pull → cached parquet (ABSOLUTE CN, RNA-seq TPM, metadata) |
| `02_absolute_codel.R` | MTAP homdel/hetdel frequency per TCGA cohort (ABSOLUTE-corrected) |
| `02b_fetch_rnaseq.R` | recount3 RNA-seq fetch → parquet |
| `03_cn_expression.R` | CN → expression fidelity per cohort (log2 TPM+1 vs ABSOLUTE CN bin) |
| `03b_fetch_maf.R` | MC3 public MAF fetch → parquet |
| `04_cooccurrence.R` | Co-occurrence / mutual exclusivity across driver genes |
| `04a_codeletion.R` | 9p21.3 co-deletion partners in MTAP-homdel patients |
| `04b_mtap_pop_mutations.R` | Mutational landscape of MTAP-homdel patients per cohort |
| `05_chrom_context.R` | Deletion focality (focal vs arm-level) at 9p21.3 |
| `06_validation_msk.R` | MSK-IMPACT cross-cohort validation via cBioPortal |
| `12_render_dashboard.R` | Assembles all parquets + YAML data files → single-page HTML dashboard |
| `install_deps.R` | One-time R package installation |
| `qc_check.R` | QC diagnostics — run after data fetch to verify sample counts |
| `utils/audit.R` | Writes provenance rows to `results/audit.parquet` |
| `utils/style.R` | Shared colour palette and CSS token helpers |

## Data flow

```
01–06 (fetch + analysis scripts)
  → results/*.parquet  +  data/*.yaml (hand-curated content)
    → 12_render_dashboard.R
      → results/target_intel_dashboard.html
```

Scripts 01–06 only need to be re-run if the underlying data changes.
`12_render_dashboard.R` re-reads parquets + YAML each time and is fast (~30 s).

## Key conventions

- Copy number: ABSOLUTE algorithm (Taylor 2018 PanCanAtlas), purity/ploidy-corrected, hg19
- RNA expression: log2(TPM+1) throughout
- All 33 TCGA indications included — never subset silently
- `library(SummarizedExperiment)` masks `fs::path` — always qualify as `fs::path()`
- Glue templates use `.open="{"`, `.close="}"` to avoid conflict with CSS `{}`
