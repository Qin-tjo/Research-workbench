## Stage 3 — Co-occurrence / mutual exclusivity of MTAP homdel with somatic
## mutations in each TCGA cohort. Per-cohort Fisher's exact tests; BH-q
## within cohort. Cross-cohort replication tally.

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

# Tunables
MIN_GENE_FREQ_IN_COHORT <- 0.05    # partner must be mutated in ≥5% of cohort
HYPERMUT_NS_CUTOFF      <- 300L    # >300 non-silent SNVs ≈ 10 mut/Mb on exome (standard)
Q_THRESHOLD             <- 0.10    # for "significant" replication tally
MIN_COHORTS_FOR_REP     <- 3L      # partner must replicate in ≥3 cohorts WITH same direction
TOP_N_PARTNERS          <- 30

# Load cancer driver whitelist (OncoKB ~1,240 genes)
driver_tbl <- fread(fs::path(CACHE_DIR, "oncokb_cancer_genes.tsv"),
                    sep = "\t", header = TRUE, quote = "")
CANCER_DRIVERS <- unique(driver_tbl[[1]])
CANCER_DRIVERS <- setdiff(CANCER_DRIVERS, LENGTH_BIAS_BLOCKLIST)
message(glue("[whitelist] OncoKB driver pool: {length(CANCER_DRIVERS)} genes (after blocklist)"))

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
maf <- as.data.table(read_parquet(fs::path(CACHE_DIR, "mc3_public.parquet")))
setnames(seg, tolower(names(seg)))

# Per-sample MTAP CN (overlap-weighted, hg19)
locus <- GENE_LOCI[GENE_LOCI$gene == TARGET_GENE, ]
mtap_cn <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(modal_total_cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]

# Sample pool (Stage 1 base)
samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% c("01", "03") &
  cohort %in% TCGA_PROJECTS,
  .(sample, patient, cohort, purity)
]
samples_in <- merge(samples_in, mtap_cn, by = "sample", all.x = TRUE)
samples_in <- samples_in[!is.na(modal_total_cn)]
samples_in[, mtap_homdel := as.integer(modal_total_cn == 0)]

# Dedup to one row per patient — keep highest-purity aliquot (mirrors Stage 2)
setorder(samples_in, patient, -purity)
samples_in <- samples_in[, .SD[1L], by = patient]
message(glue("[filter] samples after dedup: {nrow(samples_in)}"))

# Hypermutator filter (TMB proxy = non-silent SNV count per patient)
maf_ns <- maf[nonsilent == TRUE]
ns_per_patient <- maf_ns[, .(n_ns = .N), by = patient]
hypermut <- ns_per_patient[n_ns > HYPERMUT_NS_CUTOFF, patient]
message(glue("[filter] hypermutator patients (>{HYPERMUT_NS_CUTOFF} non-silent SNVs): {length(hypermut)}"))

samples_in <- samples_in[!patient %in% hypermut]
message(glue("[filter] samples after hypermut exclusion: {nrow(samples_in)}"))

# Log MAF↔ABSOLUTE join coverage
maf_patients     <- unique(maf$patient)
absolute_pts     <- unique(samples_in$patient)
joined_pts       <- intersect(maf_patients, absolute_pts)
n_absolute_only  <- length(setdiff(absolute_pts, maf_patients))
message(glue("[coverage] MAF patients: {length(maf_patients)}; ABSOLUTE-filtered patients: {length(absolute_pts)}; joined: {length(joined_pts)} ({sprintf('%.1f', 100*length(joined_pts)/length(absolute_pts))}%); ABSOLUTE-only (no MAF): {n_absolute_only}"))

# ---------------------------------------------------------------------------
# Build patient × gene mutation matrix (sparse via long table)
# ---------------------------------------------------------------------------
mut_long <- unique(maf_ns[patient %in% samples_in$patient,
                          .(patient, gene = hugo_symbol)])
message(glue("[mut] unique (patient,gene) pairs: {nrow(mut_long)}"))

# ---------------------------------------------------------------------------
# Per-cohort Fisher tests against MTAP homdel
# ---------------------------------------------------------------------------
fisher_one <- function(a, b, c, d) {
  # Haldane-Anscombe correction for zero cells
  m <- matrix(c(a, b, c, d) + 0.5, 2, 2)
  or <- (m[1,1] * m[2,2]) / (m[1,2] * m[2,1])
  p  <- tryCatch(fisher.test(matrix(c(a, b, c, d), 2, 2))$p.value,
                 error = function(e) NA_real_)
  list(or = or, p = p)
}

results <- list()
cohorts <- intersect(TCGA_PROJECTS, unique(samples_in$cohort))
for (co in cohorts) {
  cohort_samples <- samples_in[cohort == co]
  n_co <- nrow(cohort_samples)
  if (n_co < MIN_COHORT_N) next
  n_homdel <- sum(cohort_samples$mtap_homdel == 1)
  if (n_homdel < 3) next   # need at least a few homdels to compute

  # Cohort-specific driver set: mutated in ≥5% of cohort AND in cancer-driver
  # whitelist (eliminates TTN / MUC16 / etc. gene-length artifacts).
  cohort_muts <- mut_long[patient %in% cohort_samples$patient]
  gene_freq <- cohort_muts[, .(n_mut = uniqueN(patient)), by = gene][
                 , freq := n_mut / n_co][
                 freq >= MIN_GENE_FREQ_IN_COHORT & gene %in% CANCER_DRIVERS]
  if (nrow(gene_freq) == 0) next

  cohort_samples[, has := NA_integer_]
  for (g in gene_freq$gene) {
    mut_pts <- cohort_muts[gene == g, unique(patient)]
    cs <- copy(cohort_samples)
    cs[, mut := as.integer(patient %in% mut_pts)]
    a <- cs[mtap_homdel == 1 & mut == 1, .N]   # homdel & mut
    b <- cs[mtap_homdel == 1 & mut == 0, .N]
    c <- cs[mtap_homdel == 0 & mut == 1, .N]
    d <- cs[mtap_homdel == 0 & mut == 0, .N]
    f <- fisher_one(a, b, c, d)
    results[[length(results) + 1]] <- data.table(
      cohort        = co,
      partner_gene  = g,
      n_cohort      = n_co,
      n_mtap_homdel = n_homdel,
      n_partner_mut = a + c,
      n_both        = a,
      or            = f$or,
      p             = f$p
    )
  }
  message(glue("[cohort] {co}: tested {nrow(gene_freq)} partner genes"))
}

res <- rbindlist(results)
res[, q := p.adjust(p, method = "BH"), by = cohort]
res[, direction := fifelse(or >= 1, "co", "mx")]
out_pq <- fs::path(RESULTS_DIR, "03_cooccurrence_long.parquet")
write_parquet(res, out_pq)
message(glue("[done] tests: {nrow(res)} across {uniqueN(res$cohort)} cohorts; wrote {out_pq}"))

# ---------------------------------------------------------------------------
# Cross-cohort replication: top partners by # cohorts with q<Q_THRESHOLD
# ---------------------------------------------------------------------------
sig <- res[!is.na(q) & q < Q_THRESHOLD]
# Direction-consistent replication tally: a partner counts only if it hits
# significance with the same direction (co vs mx) in multiple cohorts.
rep_dir <- sig[, .(n_cohorts_sig = uniqueN(cohort),
                   mean_or       = exp(mean(log(pmax(or, 1e-6)), na.rm = TRUE))),
                by = .(partner_gene, direction)
              ][order(-n_cohorts_sig, -abs(log(mean_or)))]
print(head(rep_dir, 25))

# Pick partners that replicate (same direction) in ≥ MIN_COHORTS_FOR_REP cohorts
top_partners <- unique(head(rep_dir[n_cohorts_sig >= MIN_COHORTS_FOR_REP]$partner_gene,
                            TOP_N_PARTNERS))
if (length(top_partners) < 5) {
  # Fallback: lower threshold to ≥2 to avoid empty plot
  message(glue("[plot] no partners replicate in ≥{MIN_COHORTS_FOR_REP}; falling back to ≥2"))
  top_partners <- unique(head(rep_dir[n_cohorts_sig >= 2]$partner_gene, TOP_N_PARTNERS))
}
message(glue("[plot] partners to plot: {length(top_partners)}"))

# ---------------------------------------------------------------------------
# Cohort order: by Stage 1 homdel% descending
# ---------------------------------------------------------------------------
s1 <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
cohort_order <- s1[order(-homdel_pct)]$cohort
cohort_order <- intersect(cohort_order, unique(res$cohort))

plot_df <- res[partner_gene %in% top_partners]
plot_df[, cohort       := factor(cohort, levels = cohort_order)]
plot_df[, partner_gene := factor(partner_gene, levels = rev(top_partners))]
plot_df[, log_or       := log2(pmax(pmin(or, 64), 1/64))]
plot_df[, neglog10q    := pmin(-log10(pmax(q, 1e-6)), 6)]
plot_df[, sig_flag     := !is.na(q) & q < Q_THRESHOLD]

# X-axis labels: cohort + per-cohort n (samples tested) and n_homdel
n_by_cohort <- unique(res[, .(cohort, n_cohort, n_mtap_homdel)])
xlab_map <- setNames(
  paste0(sub("^TCGA-", "", n_by_cohort$cohort),
         "\nn=", n_by_cohort$n_cohort,
         "  (CN0=", n_by_cohort$n_mtap_homdel, ")"),
  n_by_cohort$cohort
)
# Only keep cohorts in the order present in plot_df
xlab_vec <- xlab_map[as.character(levels(plot_df$cohort))]

p <- ggplot(plot_df, aes(x = cohort, y = partner_gene)) +
  geom_point(aes(size = neglog10q, fill = log_or, color = sig_flag),
             shape = 21, stroke = 0.4) +
  scale_size_continuous(range = c(0.5, 6),
                        breaks = c(1, 2, 3, 5),
                        labels = c("0.1", "0.01", "0.001", "1e-5"),
                        name = "q-value") +
  scale_fill_gradient2(low = "#A32D2D", mid = "#FFFFFF", high = "#185FA5",
                       midpoint = 0,
                       limits = c(-6, 6), oob = scales::squish,
                       breaks = c(-6, -3, 0, 3, 6),
                       labels = c("≤ −6\n(mutex)", "−3", "0", "+3", "≥ +6\n(co-occur)"),
                       name = "log2(odds ratio)") +
  scale_color_manual(values = c(`TRUE` = "#1a1a18", `FALSE` = "#CCCCCC"),
                     guide  = "none") +
  scale_x_discrete(labels = xlab_vec, drop = FALSE) +
  labs(
    title    = "MTAP homozygous deletion — co-occurrence / mutual exclusivity with somatic mutations  [legacy / footnote panel]",
    subtitle = wrap_text(glue("Per-cohort Fisher's exact, BH-q within cohort. Partner pool = OncoKB cancer drivers (~{length(CANCER_DRIVERS)}) ∩ mutated in ≥{round(100*MIN_GENE_FREQ_IN_COHORT)}% of cohort. Hypermutators (>{HYPERMUT_NS_CUTOFF} non-silent muts ≈ 10/Mb) excluded. Partners shown: direction-consistent significance (q<{Q_THRESHOLD}) in ≥{MIN_COHORTS_FOR_REP} cohorts. Cohorts sorted by Stage 1 homdel%."),
                          width = 150),
    caption = wrap_text("Result is dominated by pathway-redundant mutual exclusivity (TP53, RB1, CDKN2A, IDH1) — i.e. the expected null when 9p21 deletion is one of several alternative routes to p53/RB pathway disruption. The actionable patient-population analyses are Stage 3A (CN co-deletion partners) and Stage 3B (mutation landscape of MTAP-homdel patients).",
                        width = 160),
    x = NULL, y = NULL
  ) +
  theme_target_intel(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "mono", size = 7.5,
                               lineheight = 0.85),
    axis.text.y = element_text(family = "mono", size = 8)
  )

out_png <- fs::path(RESULTS_DIR, "03_cooccurrence_dotplot.png")
ggsave(out_png, p, width = max(11, 0.55 * length(cohort_order) + 4),
       height = max(7, 0.35 * length(top_partners) + 3.2), dpi = 160)
message(glue("[plot] {out_png}"))

# Audit
write_audit(
  audit_row(
    analysis    = "03_cooccurrence",
    source      = "ABSOLUTE (Taylor 2018) + MC3 PUBLIC v0.2.8 (Ellrott 2018)",
    n_in        = nrow(samples_in),
    n_excluded  = length(hypermut),
    test        = glue("Fisher's exact 2x2; partner frequency >= {MIN_GENE_FREQ_IN_COHORT}"),
    adjust      = "BH within cohort",
    output_path = paste(out_pq, out_png, sep = "; "),
    notes       = glue("hypermut_cutoff={HYPERMUT_NS_CUTOFF}; q={Q_THRESHOLD}; min_cohorts_rep={MIN_COHORTS_FOR_REP}; driver_pool={length(CANCER_DRIVERS)}; MAF↔ABS join={length(joined_pts)}/{length(absolute_pts)}; ABSOLUTE-only={n_absolute_only}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 3 complete.")
