## Stage 4 — Focality of MTAP deletion at 9p21.3, per TCGA cohort.
## "When MTAP is deleted, is it focal (driver-selected) or arm-level (passenger)?"

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

MIN_DEL_FOR_PANEL <- 5L     # cohort needs ≥5 MTAP-deleted samples per stratum

# Focality cut-points (Mb).
FOCAL_MAX_MB <- 3
INTER_MAX_MB <- 25
# chr9 p-arm length (hg19) — centromere starts at ~chr9:47.4 Mb
P_ARM_LEN_MB <- 49.0

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
setnames(seg, tolower(names(seg)))

# Sample pool — Stage 1 base, one row per patient (highest purity)
samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% c("01", "03") &
  cohort %in% TCGA_PROJECTS,
  .(sample, patient, cohort, purity)
]
setorder(samples_in, patient, -purity)
samples_in <- samples_in[, .SD[1L], by = patient]

# Per-sample MTAP CN (overlap-weighted, same as Stage 1)
locus <- GENE_LOCI[GENE_LOCI$gene == TARGET_GENE, ]
mtap_cn <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(modal_total_cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]
samples_in <- merge(samples_in, mtap_cn, by = "sample", all.x = TRUE)
samples_in <- samples_in[!is.na(modal_total_cn) & modal_total_cn <= 1]
samples_in[, mtap_class := fifelse(modal_total_cn == 0, "homdel", "hetdel")]
message(glue("[filter] MTAP-deleted patients (CN ≤ 1): {nrow(samples_in)}  ",
             "(homdel={sum(samples_in$mtap_class=='homdel')}, hetdel={sum(samples_in$mtap_class=='hetdel')})"))

# ---------------------------------------------------------------------------
# Compute deletion footprint per sample
# ---------------------------------------------------------------------------
# For each deleted sample, find ABSOLUTE segments on chr9 whose CN equals the
# sample's MTAP CN call AND that overlap or chain contiguously through the
# MTAP locus. Merge adjacent same-CN segments (allow ≤500 kb gap to bridge
# microsegmentation), then take the span of the merged interval containing
# MTAP as the deletion footprint length.
GAP_MERGE_BP <- 500000L

chr9_seg <- seg[as.character(chromosome) == "9", .(sample, start, end, modal_total_cn)]
setkey(chr9_seg, sample, start)

# Helper: for one sample's chr9 segs, find merged interval covering MTAP at the
# target CN call.
footprint_for_sample <- function(sg, target_cn) {
  sg <- sg[modal_total_cn == target_cn]
  if (nrow(sg) == 0) return(NA_real_)
  setorder(sg, start)
  # Merge adjacent same-CN intervals separated by ≤ GAP_MERGE_BP
  merged_start <- sg$start[1]
  merged_end   <- sg$end[1]
  cluster_id   <- 1L
  cluster      <- rep(NA_integer_, nrow(sg))
  cluster[1]   <- 1L
  for (i in seq_len(nrow(sg))[-1]) {
    if (sg$start[i] - merged_end <= GAP_MERGE_BP) {
      merged_end <- max(merged_end, sg$end[i])
    } else {
      cluster_id <- cluster_id + 1L
      merged_start <- sg$start[i]
      merged_end   <- sg$end[i]
    }
    cluster[i] <- cluster_id
  }
  sg[, cluster := cluster]
  merged <- sg[, .(start = min(start), end = max(end)), by = cluster]
  # Pick the merged interval that overlaps the MTAP locus
  hits <- merged[start <= locus$end & end >= locus$start]
  if (nrow(hits) == 0) return(NA_real_)
  with(hits[1], (end - start + 1) / 1e6)
}

samples_in[, deletion_mb := mapply(
  footprint_for_sample,
  sg        = lapply(sample, function(s) chr9_seg[sample == s]),
  target_cn = modal_total_cn
)]

# Classify focality
samples_in[, focality := fcase(
  is.na(deletion_mb),                                "unknown",
  deletion_mb < FOCAL_MAX_MB,                        "focal",
  deletion_mb < INTER_MAX_MB,                        "intermediate",
  default                                          = "arm-level"
)]
samples_in[, focality := factor(focality, levels = c("focal", "intermediate", "arm-level", "unknown"))]

# Tag fully-arm-spanning deletions as arm-level even if <50 Mb (e.g. p-arm only)
samples_in[focality == "intermediate" & deletion_mb >= P_ARM_LEN_MB * 0.5,
           focality := "arm-level"]

# Save per-sample classification
sample_pq <- fs::path(RESULTS_DIR, "04_focality_per_sample.parquet")
write_parquet(samples_in[, .(sample, patient, cohort, mtap_class, modal_total_cn,
                              deletion_mb, focality)], sample_pq)

# ---------------------------------------------------------------------------
# Per (cohort, mtap_class) tabulation
# ---------------------------------------------------------------------------
per_co <- samples_in[focality != "unknown",
  .(
    n_samples       = .N,
    median_mb       = median(deletion_mb, na.rm = TRUE),
    pct_focal       = 100 * sum(focality == "focal")        / .N,
    pct_intermediate= 100 * sum(focality == "intermediate") / .N,
    pct_arm         = 100 * sum(focality == "arm-level")    / .N
  ),
  by = .(cohort, mtap_class)
]

# Drop strata with too few samples
per_co_plot <- per_co[n_samples >= MIN_DEL_FOR_PANEL]
out_pq <- fs::path(RESULTS_DIR, "04_focality_per_cohort.parquet")
write_parquet(per_co, out_pq)
print(per_co_plot[order(mtap_class, -pct_focal)])

# ---------------------------------------------------------------------------
# Plot 4A — stacked horizontal bar of focality %
# ---------------------------------------------------------------------------
# Cohort order by Stage 1 homdel%
s1 <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
cohort_order <- intersect(s1[order(-homdel_pct)]$cohort, unique(per_co_plot$cohort))

long <- melt(per_co_plot,
             id.vars       = c("cohort", "mtap_class", "n_samples", "median_mb"),
             measure.vars  = c("pct_focal", "pct_intermediate", "pct_arm"),
             variable.name = "focality", value.name = "pct")
long[, focality := factor(focality,
  levels = c("pct_arm", "pct_intermediate", "pct_focal"),
  labels = c("Arm-level (≥ 25 Mb)", "Intermediate (3–25 Mb)", "Focal (< 3 Mb)"))]

long[, cohort     := factor(cohort, levels = rev(cohort_order))]
long[, mtap_class := factor(mtap_class, levels = c("hetdel", "homdel"),
                            labels = c("Heterozygous deletion (CN=1)",
                                       "Homozygous deletion (CN=0)"))]

# Right-margin labels per (cohort, stratum)
label_df <- per_co_plot[, .(cohort     = factor(cohort, levels = rev(cohort_order)),
                            mtap_class = factor(mtap_class, levels = c("hetdel","homdel"),
                                                labels = c("Heterozygous deletion (CN=1)",
                                                           "Homozygous deletion (CN=0)")),
                            x          = 102,
                            label      = sprintf("n=%d • %.1f Mb",
                                                  n_samples, median_mb))]

p1 <- ggplot(long, aes(x = pct, y = cohort, fill = focality)) +
  geom_col(width = 0.78, color = "white", linewidth = 0.2) +
  geom_text(data = label_df, aes(x = x, y = cohort, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2.7, color = "#333333",
            family = "mono") +
  facet_wrap(~ mtap_class, ncol = 2) +
  scale_fill_manual(values = c(
    "Focal (< 3 Mb)"          = "#0F6E56",  # dark teal — driver-like
    "Intermediate (3–25 Mb)"  = "#FAC775",  # amber
    "Arm-level (≥ 25 Mb)"     = "#A32D2D"   # red — passenger-like
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 140),
                     breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title    = "Focality of MTAP deletion across TCGA cohorts",
    subtitle = wrap_text(glue("Per cohort and CN class, fraction of MTAP-deleted samples whose contiguous chr9p deletion footprint falls into each focality bin. Footprint computed by merging adjacent ABSOLUTE same-CN segments (≤500 kb gap). Cohort rows show n samples • median footprint Mb. Cohorts sorted by Stage 1 homdel% (top → bottom). Bins: focal < {FOCAL_MAX_MB} Mb (likely driver-selected) | intermediate 3–{INTER_MAX_MB} Mb | arm-level ≥ {INTER_MAX_MB} Mb (or ≥ 50% of 9p, likely passenger)."),
                          width = 145),
    x        = "% of deleted samples",
    y        = NULL,
    fill     = NULL,
    caption  = glue("Strata shown: ≥{MIN_DEL_FOR_PANEL} deleted samples in that CN class. Source: ABSOLUTE PanCanAtlas (Taylor 2018, hg19).")
  ) +
  theme_target_intel(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = "left",
    legend.key.size    = unit(11, "pt"),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(family = "mono"),
    strip.text         = element_text(face = "bold", size = 10)
  )

out_png_4a <- fs::path(RESULTS_DIR, "04_focality_stacked.png")
ggsave(out_png_4a, p1,
       width  = 13,
       height = 0.32 * length(cohort_order) + 3.0, dpi = 160)

# ---------------------------------------------------------------------------
# Plot 4B — deletion size distribution (violins) by cohort × class
# ---------------------------------------------------------------------------
size_df <- samples_in[focality != "unknown" &
                       cohort %in% per_co_plot$cohort]
size_df[, cohort     := factor(cohort, levels = rev(cohort_order))]
size_df[, mtap_class := factor(mtap_class, levels = c("hetdel", "homdel"),
                                labels = c("Heterozygous deletion (CN=1)",
                                           "Homozygous deletion (CN=0)"))]

p2 <- ggplot(size_df, aes(x = deletion_mb, y = cohort)) +
  geom_vline(xintercept = c(FOCAL_MAX_MB, INTER_MAX_MB),
             linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_violin(aes(fill = mtap_class), color = NA, alpha = 0.85,
              scale = "width", trim = TRUE, width = 0.85) +
  geom_jitter(width = 0, height = 0.18, size = 0.4, alpha = 0.5, color = "#1a1a18") +
  facet_wrap(~ mtap_class, ncol = 2) +
  scale_x_log10(breaks = c(0.1, 1, 3, 10, 25, 100),
                labels = c("0.1", "1", "3", "10", "25", "100"),
                limits = c(0.05, 150)) +
  scale_fill_manual(values = c(
    "Heterozygous deletion (CN=1)" = "#9FE1CB",
    "Homozygous deletion (CN=0)"   = "#0F6E56"
  ), guide = "none") +
  labs(
    title    = "Distribution of MTAP deletion footprint size",
    subtitle = wrap_text("Per-sample contiguous chr9p deletion footprint (log10 Mb). Dashed vertical lines mark the focal/intermediate (3 Mb) and intermediate/arm-level (25 Mb) cut-points. Each row is a TCGA cohort; cohorts ordered as in panel A.",
                          width = 145),
    x        = "Deletion footprint (Mb, log10 scale)",
    y        = NULL,
    caption  = glue("Strata shown: ≥{MIN_DEL_FOR_PANEL} deleted samples in that CN class.")
  ) +
  theme_target_intel(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(family = "mono"),
    strip.text         = element_text(face = "bold", size = 10)
  )

out_png_4b <- fs::path(RESULTS_DIR, "04_focality_violins.png")
ggsave(out_png_4b, p2,
       width  = 13,
       height = 0.32 * length(cohort_order) + 3.0, dpi = 160)

# Audit
write_audit(
  audit_row(
    analysis    = "04_focality",
    source      = "ABSOLUTE PanCanAtlas (Taylor 2018, hg19)",
    n_in        = nrow(samples_in),
    n_excluded  = sum(samples_in$focality == "unknown"),
    test        = "descriptive (% by focality bin)",
    adjust      = "none",
    output_path = paste(out_pq, sample_pq, out_png_4a, out_png_4b, sep = "; "),
    notes       = glue("focal<{FOCAL_MAX_MB}Mb; intermediate<{INTER_MAX_MB}Mb; arm-level≥{INTER_MAX_MB}Mb; gap_merge=500kb")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 4 complete.")
