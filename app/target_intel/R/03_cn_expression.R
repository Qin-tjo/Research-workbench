## Stage 2 — MTAP CN bin vs. log2(TPM+1) expression, per TCGA cohort.

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
# Inputs: per-sample CN class (from Stage 1 logic) + MTAP TPM (Stage 2a fetch)
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
tpm <- as.data.table(read_parquet(fs::path(CACHE_DIR, "rnaseq_mtap_tpm.parquet")))

setnames(seg, tolower(names(seg)))

# Sample filter (Stage 1 base + purity ≥ PURITY_MIN)
samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% c("01", "03") &
  cohort %in% TCGA_PROJECTS &
  !is.na(purity) & purity >= PURITY_MIN,
  .(sample, patient, cohort, purity, ploidy)
]
n_before_dedup <- nrow(samples_in)
# Deduplicate to ONE row per patient — keep the aliquot with highest ABSOLUTE purity
# (best-informed CN call). Ties broken by first occurrence.
setorder(samples_in, patient, -purity)
samples_in <- samples_in[, .SD[1L], by = patient]
n_after_dedup  <- nrow(samples_in)
message(glue("[dedup] one row per patient: {n_after_dedup} / {n_before_dedup}"))

# Per-sample MTAP CN — overlap-length-weighted CN across all segments touching
# the gene body. Avoids the biased "take the most-deleted segment" shortcut.
locus <- GENE_LOCI[GENE_LOCI$gene == TARGET_GENE, ]
seg_mtap <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(modal_total_cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]
samples_in <- merge(samples_in, seg_mtap, by = "sample", all.x = TRUE)
samples_in <- samples_in[!is.na(modal_total_cn)]
samples_in[, cn_bin := fifelse(modal_total_cn == 0, "0",
                       fifelse(modal_total_cn == 1, "1",
                       fifelse(modal_total_cn == 2, "2", "3+")))]
samples_in[, cn_bin := factor(cn_bin, levels = c("0", "1", "2", "3+"))]

# Join TPM by patient barcode (12 chars). recount3 may have multiple aliquots per
# patient → average their log2(TPM+1). ABSOLUTE side is already deduped to one
# aliquot per patient (highest-purity).
tpm[, patient := substr(external_id, 1, 12)]
# Restrict TPM to primary-tumor sample types (positions 14-15)
tpm[, sample_type := substr(external_id, 14, 15)]
tpm_primary <- tpm[sample_type %in% c("01", "03")]
tpm_collapsed <- tpm_primary[, .(log2_tpm_plus1 = mean(log2_tpm_plus1, na.rm = TRUE)),
                              by = patient]

joined <- merge(samples_in, tpm_collapsed, by = "patient", all.x = TRUE)

data <- joined[!is.na(log2_tpm_plus1),
               .(sample, patient, cohort, purity, modal_total_cn, cn_bin, log2_tpm_plus1)]
message(glue("[join] samples with CN + TPM + purity≥{PURITY_MIN}: {nrow(data)}"))

# Cohort-level filter: n ≥ MIN_COHORT_N (do NOT require CN=0 samples — include
# all 33 cohorts; ρ becomes NA / unstable when n_cn0 is very small, which we flag).
cohort_eligibility <- data[, .(
  n_total = .N,
  n_cn0   = sum(cn_bin == "0")
), by = cohort]
keep <- cohort_eligibility[n_total >= MIN_COHORT_N, cohort]
excluded <- cohort_eligibility[!cohort %in% keep]
data_keep <- data[cohort %in% keep]
message(glue("[filter] cohorts kept (n >= {MIN_COHORT_N}): {length(keep)} / {nrow(cohort_eligibility)}"))
print(excluded[order(-n_total)])

# ---------------------------------------------------------------------------
# Per-cohort summary stats
# ---------------------------------------------------------------------------
per_cohort <- data_keep[, {
  n_cn0_here <- sum(cn_bin == "0")
  # ρ is only meaningful when there are ≥3 CN=0 samples AND CN bin has variation.
  # Otherwise the metric just summarises noise across CN=1/2/3+ and is misleading.
  rho   <- NA_real_; slope <- NA_real_; r2 <- NA_real_
  if (n_cn0_here >= 3 && length(unique(modal_total_cn)) >= 2) {
    rho   <- suppressWarnings(cor(as.numeric(modal_total_cn), log2_tpm_plus1,
                                  method = "spearman"))
    fit   <- lm(log2_tpm_plus1 ~ modal_total_cn)
    slope <- unname(coef(fit)[2])
    r2    <- summary(fit)$r.squared
  }
  # Penetrance metric: fraction of CN=0 samples below the 10th percentile of the
  # CN=2 (neutral) reference distribution. Non-circular even when CN=0 is common.
  ref   <- log2_tpm_plus1[cn_bin == "2"]
  cn0v  <- log2_tpm_plus1[cn_bin == "0"]
  pen   <- if (length(ref) >= 5 && length(cn0v) >= 1)
             mean(cn0v < quantile(ref, 0.10, names = FALSE))
           else NA_real_

  list(
    n                       = .N,
    n_cn0                   = n_cn0_here,
    n_cn1                   = sum(cn_bin == "1"),
    n_cn2                   = sum(cn_bin == "2"),
    n_cn3plus               = sum(cn_bin == "3+"),
    spearman_rho            = rho,
    slope                   = slope,
    r2                      = r2,
    frac_cn0_below_cn2_p10  = pen,
    median_cn0              = median(log2_tpm_plus1[cn_bin == "0"], na.rm = TRUE),
    median_cn1              = median(log2_tpm_plus1[cn_bin == "1"], na.rm = TRUE),
    median_cn2              = median(log2_tpm_plus1[cn_bin == "2"], na.rm = TRUE),
    median_cn3p             = median(log2_tpm_plus1[cn_bin == "3+"], na.rm = TRUE)
  )
}, by = cohort][order(-spearman_rho, na.last = TRUE)]   # highest positive rho first; NA at bottom

print(per_cohort)
out_pq <- fs::path(RESULTS_DIR, "02_cn_expression_per_cohort.parquet")
write_parquet(per_cohort, out_pq)

# ---------------------------------------------------------------------------
# Preview 1: 33-panel small-multiple boxplot
# ---------------------------------------------------------------------------
# Build per-cohort label and join in (avoids match() scoping pitfalls)
cohort_labels <- per_cohort[, .(
  cohort,
  panel_label = paste0(sub("^TCGA-", "", cohort),
                       "  ρ=", ifelse(is.na(spearman_rho), "NA", sprintf("%.2f", spearman_rho)),
                       "  n=", n,
                       "  CN0=", n_cn0)
)]
data_keep <- merge(data_keep, cohort_labels, by = "cohort")
data_keep[, panel_label := factor(panel_label, levels = cohort_labels$panel_label)]

# Per-panel, per-CN-bin counts for labelling
n_per_bin <- data_keep[, .N, by = .(panel_label, cn_bin)]
y_range   <- range(data_keep$log2_tpm_plus1, na.rm = TRUE)
y_label   <- y_range[1] - 0.06 * diff(y_range)   # just below the panel

p1 <- ggplot(data_keep, aes(x = cn_bin, y = log2_tpm_plus1)) +
  geom_boxplot(aes(fill = cn_bin), outlier.size = 0.4, lwd = 0.3, width = 0.7) +
  geom_text(data = n_per_bin, aes(x = cn_bin, y = y_label, label = paste0("n=", N)),
            inherit.aes = FALSE, size = 2.2, color = "grey35") +
  facet_wrap(~ panel_label, ncol = 6) +
  coord_cartesian(ylim = c(y_label, y_range[2]), clip = "off") +
  scale_fill_manual(values = c(
    "0"  = "#0F6E56",
    "1"  = "#5DCAA5",
    "2"  = "#D3D1C7",
    "3+" = "#FAC775"
  ), guide = "none") +
  labs(
    title    = "MTAP copy-number bin vs. expression — by TCGA cohort",
    subtitle = wrap_text(glue("ABSOLUTE total CN (PanCanAtlas, hg19) vs. log2(TPM+1) from recount3. Only samples with BOTH ABSOLUTE CN and RNA-seq TPM available. Filters: purity ≥ {PURITY_MIN}, n ≥ {MIN_COHORT_N}, one row per patient (highest-purity aliquot). Panels sorted by Spearman ρ; ρ shown only where ≥3 CN=0 samples (else 'NA')."),
                          width = 140),
    x        = "MTAP allelic CN (ABSOLUTE)",
    y        = "log2(TPM+1)"
  ) +
  theme_target_intel(base_size = 10) +
  theme(
    strip.text    = element_text(size = 8, face = "bold", color = "#1a1a18"),
    panel.spacing = unit(8, "pt")
  )

out_png1 <- fs::path(RESULTS_DIR, "02_cn_expression_boxplots.png")
n_panels <- length(keep)
ggsave(out_png1, p1, width = 14,
       height = ceiling(n_panels / 6) * 2.1 + 2.2, dpi = 160)
message(glue("[plot] {out_png1}"))

# Audit
write_audit(
  audit_row(
    analysis    = "02_cn_expression",
    source      = "ABSOLUTE (Taylor 2018) + recount3 TCGA TPM",
    n_in        = nrow(data_keep),
    n_excluded  = nrow(data) - nrow(data_keep),
    test        = "Spearman ρ; linear fit y ~ cn",
    adjust      = "none",
    output_path = paste(out_pq, out_png1, sep = "; "),
    notes       = glue("purity_min={PURITY_MIN}; cohorts kept={length(keep)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 2 complete.")
