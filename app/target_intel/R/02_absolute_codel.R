## Stage 1 — MTAP deletion frequency by TCGA cohort.
## Per-sample MTAP CN from ABSOLUTE segments; per-cohort homdel/hetdel %.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(glue)
  library(fs)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))
source(file.path(SCRIPT_DIR, "utils/style.R"))

# ---------------------------------------------------------------------------
# Load cached data
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))

# Standardise seg column names
setnames(seg, tolower(names(seg)))
stopifnot(all(c("sample","chromosome","start","end","modal_total_cn") %in% names(seg)))

# ---------------------------------------------------------------------------
# Sample filtering
# ---------------------------------------------------------------------------
# Primary tumor: 01 = solid primary; 03 = blood-derived primary (LAML).
keep_sample_types <- c("01", "03")

samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% keep_sample_types &
  cohort %in% TCGA_PROJECTS,
  .(sample, patient, cohort, sample_type, purity, ploidy)
]
n_total   <- nrow(pur)
n_kept    <- nrow(samples_in)
n_excl    <- n_total - n_kept
message(glue("[filter] samples kept: {n_kept} / {n_total}  ( excluded {n_excl} )"))

# ---------------------------------------------------------------------------
# Per-sample MTAP CN call
# ---------------------------------------------------------------------------
locus <- GENE_LOCI[GENE_LOCI$gene == TARGET_GENE, ]
stopifnot(nrow(locus) == 1)

# Per-sample MTAP CN — overlap-length-weighted across all segments touching
# the gene body (avoids the biased "most-deleted segment wins" shortcut).
seg_mtap <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(modal_total_cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]

samples_in <- merge(samples_in, seg_mtap, by = "sample", all.x = TRUE)
samples_in[, mtap_class := fifelse(is.na(modal_total_cn), "unknown",
                            fifelse(modal_total_cn == 0,  "homdel",
                            fifelse(modal_total_cn == 1,  "hetdel", "neutral_or_amp")))]

# ---------------------------------------------------------------------------
# Per-cohort tabulation
# ---------------------------------------------------------------------------
per_cohort <- samples_in[mtap_class != "unknown",
  .(
    n_cohort     = .N,
    n_homdel     = sum(mtap_class == "homdel"),
    n_hetdel     = sum(mtap_class == "hetdel")
  ),
  by = cohort
][n_cohort >= MIN_COHORT_N]

per_cohort[, `:=`(
  homdel_pct    = n_homdel / n_cohort * 100,
  hetdel_pct    = n_hetdel / n_cohort * 100
)]
per_cohort[, total_del_pct := homdel_pct + hetdel_pct]
setorder(per_cohort, -homdel_pct, -hetdel_pct)

print(per_cohort)

# Write result
out_pq <- fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")
write_parquet(per_cohort, out_pq)

# ---------------------------------------------------------------------------
# Preview chart
# ---------------------------------------------------------------------------
plot_df <- melt(
  per_cohort[, .(cohort, total_del_pct, homdel_pct, hetdel_pct, n_cohort)],
  id.vars       = c("cohort", "n_cohort", "total_del_pct"),
  measure.vars  = c("homdel_pct", "hetdel_pct"),
  variable.name = "class",
  value.name    = "pct"
)
plot_df[, class := factor(class,
  levels = c("hetdel_pct", "homdel_pct"),
  labels = c("Heterozygous deletion (CN=1)", "Homozygous deletion (CN=0)"))]

plot_df[, cohort := factor(cohort, levels = rev(per_cohort$cohort))]  # top = highest
label_df <- per_cohort[, .(
  cohort = factor(cohort, levels = rev(per_cohort$cohort)),
  x      = total_del_pct + 1.5,
  label  = sprintf("n=%d  •  %.1f%% / %.1f%%", n_cohort, homdel_pct, hetdel_pct)
)]

p <- ggplot(plot_df, aes(x = pct, y = cohort, fill = class)) +
  geom_col(width = 0.78) +
  geom_text(data = label_df, aes(x = x, y = cohort, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2.8, color = "#333333") +
  scale_fill_manual(values = c(
    "Homozygous deletion (CN=0)"    = "#0F6E56",  # teal-600 (dark)
    "Heterozygous deletion (CN=1)"  = "#9FE1CB"   # teal-100 (light)
  )) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.22)),
    limits = c(0, max(per_cohort$total_del_pct) * 1.45)
  ) +
  labs(
    title    = glue("MTAP deletion frequency across {nrow(per_cohort)} TCGA cohorts"),
    subtitle = wrap_text(glue("ABSOLUTE allelic CN (Taylor 2018 PanCanAtlas, hg19), overlap-weighted at the MTAP locus. Filters: primary tumour, ABSOLUTE QC pass, n ≥ {MIN_COHORT_N} per cohort. Cohorts ranked by homozygous deletion %, hetdel% as tiebreaker."),
                          width = 130),
    x        = "% of cohort samples",
    y        = NULL,
    fill     = NULL
  ) +
  theme_target_intel(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = "left",
    legend.key.size    = unit(10, "pt"),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(family = "mono")
  )

out_png <- fs::path(RESULTS_DIR, "01_mtap_deletion_freq.png")
ggsave(out_png, p, width = 10, height = 0.34 * nrow(per_cohort) + 2.0, dpi = 160)
message(glue("[plot] {out_png}"))

# Audit
write_audit(
  audit_row(
    analysis    = "01_mtap_deletion_freq",
    source      = "GDC PanCanAtlas ABSOLUTE (Taylor 2018)",
    n_in        = n_kept,
    n_excluded  = n_excl,
    test        = "none (descriptive frequencies)",
    adjust      = "none",
    output_path = paste(out_pq, out_png, sep = "; "),
    notes       = glue("cohorts kept = {nrow(per_cohort)}; target = {TARGET_GENE}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 1 complete.")
