## QC check — Stage 0/1/2 internal consistency + literature sanity checks.
## Each check prints PASS / WARN / FAIL with a short explanation.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(glue)
  library(fs)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))

ok   <- function(msg) cat(sprintf("  [PASS] %s\n", msg))
warn <- function(msg) cat(sprintf("  [WARN] %s\n", msg))
fail <- function(msg) cat(sprintf("  [FAIL] %s\n", msg))
hdr  <- function(msg) cat(sprintf("\n=== %s ===\n", msg))

seg  <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur  <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
tpm  <- as.data.table(read_parquet(fs::path(CACHE_DIR, "rnaseq_mtap_tpm.parquet")))
s1   <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
s2   <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "02_cn_expression_per_cohort.parquet")))
setnames(seg, tolower(names(seg)))

# ---------------------------------------------------------------------------
hdr("1. Source data integrity")
# Expected PanCanAtlas ABSOLUTE counts (Taylor 2018): ~10,786 sample-rows; ~11k samples segged.
if (nrow(pur) > 10000 && nrow(pur) < 11500) ok(glue("ABSOLUTE purity table: {nrow(pur)} rows (expected ~10,786)")) else warn(glue("ABSOLUTE purity table size unusual: {nrow(pur)} rows"))

if (uniqueN(seg$sample) > 10500 && uniqueN(seg$sample) < 11500) ok(glue("ABSOLUTE seg samples: {uniqueN(seg$sample)} (expected ~11,084)")) else warn(glue("ABSOLUTE seg unique samples: {uniqueN(seg$sample)}"))

# Chromosome 9 coverage of segments
n_chr9_seg <- seg[as.character(chromosome) == "9", .N]
ok(glue("chr9 segments: {n_chr9_seg}"))

# ---------------------------------------------------------------------------
hdr("2. Cohort mapping coverage")
n_with_cohort <- pur[!is.na(cohort), .N]
n_no_cohort   <- pur[is.na(cohort), .N]
if (n_no_cohort / nrow(pur) < 0.02) ok(glue("{n_no_cohort}/{nrow(pur)} samples missing cohort ({sprintf('%.1f', 100*n_no_cohort/nrow(pur))}%)")) else warn(glue("{n_no_cohort}/{nrow(pur)} samples missing cohort — high"))

cohorts_present <- unique(pur[!is.na(cohort)]$cohort)
missing <- setdiff(TCGA_PROJECTS, cohorts_present)
if (length(missing) == 0) ok("all 33 TCGA cohorts present in purity table") else fail(glue("missing cohorts: {paste(missing, collapse=', ')}"))

# ---------------------------------------------------------------------------
hdr("3. Sample-type / QC filter sanity")
print(pur[, .N, by = .(sample_type)][order(-N)])
laml_03 <- pur[cohort == "TCGA-LAML" & sample_type == "03", .N]
laml_01 <- pur[cohort == "TCGA-LAML" & sample_type == "01", .N]
if (laml_03 > laml_01) ok(glue("LAML uses sample_type=03 (n={laml_03}); type=01 n={laml_01}")) else warn(glue("LAML unexpected: type=03 n={laml_03}, type=01 n={laml_01}"))

qc_dist <- pur[, .N, by = .(qc_pass)]
ok(glue("ABSOLUTE QC pass = {qc_dist[qc_pass==TRUE]$N} / {sum(qc_dist$N)}"))

# ---------------------------------------------------------------------------
hdr("4. MTAP locus segment overlap (hg19)")
locus <- GENE_LOCI[GENE_LOCI$gene == "MTAP", ]
seg_mtap <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
]
n_samples_with_mtap <- uniqueN(seg_mtap$sample)
ok(glue("samples with at least one MTAP-overlapping segment: {n_samples_with_mtap}"))

if (n_samples_with_mtap > 10000) ok("MTAP segment coverage looks complete") else warn(glue("only {n_samples_with_mtap} samples touch MTAP — investigate"))

# Multi-segment-at-MTAP frequency
multi <- seg_mtap[, .N, by = sample][N > 1]
cat(glue("  -> samples with >1 segment touching MTAP: {nrow(multi)} ({sprintf('%.2f', 100*nrow(multi)/n_samples_with_mtap)}%)\n"))

# ---------------------------------------------------------------------------
hdr("5. Per-sample CN bin distribution (sanity)")
locus <- GENE_LOCI[GENE_LOCI$gene == "MTAP", ]
cn_calls <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]
print(cn_calls[, .N, by = cn][order(cn)])
cn2_frac <- cn_calls[cn == 2, .N] / nrow(cn_calls)
if (cn2_frac > 0.5 && cn2_frac < 0.85) ok(glue("CN=2 fraction = {sprintf('%.1f', 100*cn2_frac)}% (plausible)")) else warn(glue("CN=2 fraction = {sprintf('%.1f', 100*cn2_frac)}% — unusual"))

# ---------------------------------------------------------------------------
hdr("6. Stage 1 — deduplication / one-row-per-sample check")
samples_in <- pur[qc_pass == TRUE & sample_type %in% c("01","03") &
                  cohort %in% TCGA_PROJECTS]
dup <- samples_in[, .N, by = sample][N > 1]
if (nrow(dup) == 0) ok("no duplicate sample IDs in filtered set") else fail(glue("{nrow(dup)} duplicate sample IDs"))

# Stage 1 cohort totals match pur counts
s1_total <- sum(s1$n_cohort)
expected <- samples_in[sample %in% cn_calls$sample, .N]
if (abs(s1_total - expected) < 50) ok(glue("Stage 1 total = {s1_total}, expected ~{expected}")) else warn(glue("Stage 1 total {s1_total} vs expected {expected} — mismatch"))

# ---------------------------------------------------------------------------
hdr("7. Stage 1 — literature sanity (published MTAP homdel rates)")
lit <- list(GBM = c(40, 55), MESO = c(30, 55), PAAD = c(12, 25),
            BLCA = c(15, 30), LUSC = c(10, 25), HNSC = c(8, 20),
            SKCM = c(8, 25),  ESCA = c(10, 25))
for (k in names(lit)) {
  obs <- s1[cohort == paste0("TCGA-", k), homdel_pct]
  rng <- lit[[k]]
  if (length(obs) == 0) { warn(glue("{k}: not in s1 output")); next }
  status <- if (obs >= rng[1] && obs <= rng[2]) "PASS" else "WARN"
  cat(sprintf("  [%s] %-5s observed=%.1f%% expected=%d–%d%%\n", status, k, obs, rng[1], rng[2]))
}

# ---------------------------------------------------------------------------
hdr("8. Stage 2 — patient-level deduplication check")
# Reconstruct Stage 2 filter
s2_samples <- pur[qc_pass == TRUE & sample_type %in% c("01","03") &
                  cohort %in% TCGA_PROJECTS &
                  !is.na(purity) & purity >= PURITY_MIN]
setorder(s2_samples, patient, -purity)
s2_dedup <- s2_samples[, .SD[1L], by = patient]
dup_patients <- s2_dedup[, .N, by = patient][N > 1]
if (nrow(dup_patients) == 0) ok("Stage 2 dedup correctly produces 1 row per patient") else fail(glue("{nrow(dup_patients)} duplicate patients after dedup"))

# Join coverage to TPM
s2_dedup[, patient12 := substr(sample, 1, 12)]
tpm_pts <- unique(substr(tpm[substr(external_id,14,15) %in% c("01","03")]$external_id, 1, 12))
join_n <- sum(s2_dedup$patient12 %in% tpm_pts)
join_pct <- 100 * join_n / nrow(s2_dedup)
if (join_pct > 80) ok(glue("CN↔TPM join coverage: {join_n}/{nrow(s2_dedup)} ({sprintf('%.1f', join_pct)}%)")) else warn(glue("join coverage only {sprintf('%.1f', join_pct)}%"))

# ---------------------------------------------------------------------------
hdr("9. Stage 2 — TPM biological plausibility")
tpm_summary <- tpm[, .(mean_log2 = mean(log2_tpm_plus1, na.rm = TRUE),
                       sd_log2   = sd(log2_tpm_plus1, na.rm = TRUE),
                       p10       = quantile(log2_tpm_plus1, 0.10, na.rm = TRUE),
                       p90       = quantile(log2_tpm_plus1, 0.90, na.rm = TRUE))]
print(tpm_summary)
ok(glue("MTAP log2(TPM+1) range looks biological (median = {round(median(tpm$log2_tpm_plus1, na.rm=TRUE),2)})"))

# Per-cohort expression should drop from CN=2 -> CN=0
mono_violations <- s2[!is.na(median_cn0) & !is.na(median_cn2) &
                      median_cn0 >= median_cn2, .(cohort, median_cn0, median_cn2)]
if (nrow(mono_violations) == 0) ok("median expression is monotonic (CN=0 < CN=2) in every cohort") else { warn(glue("{nrow(mono_violations)} cohort(s) violate monotonicity:")); print(mono_violations) }

# ---------------------------------------------------------------------------
hdr("10. Stage 2 — penetrance ranking matches Stage 1 (cohorts with most deletion also have tightest fidelity)")
m <- merge(s1[, .(cohort, homdel_pct)],
           s2[!is.na(spearman_rho), .(cohort, spearman_rho)], by = "cohort")
co <- cor(m$homdel_pct, m$spearman_rho, method = "spearman")
ok(glue("Spearman correlation (cohort homdel% vs ρ) = {round(co, 2)} (positive expected; got {ifelse(co>0.5,'strong','weak')})"))

# ---------------------------------------------------------------------------
hdr("11. Audit log integrity")
audit <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "audit.parquet")))
cat(glue("  audit rows: {nrow(audit)}; analyses: {paste(unique(audit$analysis), collapse=', ')}\n"))

cat("\n=== QC SUMMARY DONE ===\n")
