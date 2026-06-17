## Stage 3B — Mutational landscape of MTAP-homdel patients, per cohort.
## "In MTAP-homdel patients, what driver mutations come along, vs cohort baseline?"

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

MIN_HOMDEL_FOR_PANEL    <- 5L      # min MTAP-homdel patients for stable rate
MIN_GENE_FREQ_BASELINE  <- 0.05    # gene must be mutated in ≥5% of cohort baseline
HYPERMUT_NS_CUTOFF      <- 300L
TOP_N_PER_COHORT        <- 10L     # top 10 drivers per cohort panel

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
maf <- as.data.table(read_parquet(fs::path(CACHE_DIR, "mc3_public.parquet")))
setnames(seg, tolower(names(seg)))

# OncoKB driver whitelist ∩ length-bias blocklist
driver_tbl     <- fread(fs::path(CACHE_DIR, "oncokb_cancer_genes.tsv"),
                        sep = "\t", header = TRUE, quote = "")
CANCER_DRIVERS <- setdiff(unique(driver_tbl[[1]]), LENGTH_BIAS_BLOCKLIST)

# Per-sample MTAP CN (overlap-weighted)
locus <- GENE_LOCI[GENE_LOCI$gene == TARGET_GENE, ]
mtap_cn <- seg[
  as.character(chromosome) == locus$chrom &
  start <= locus$end & end >= locus$start
][, overlap := pmin(end, locus$end) - pmax(start, locus$start) + 1
][, .(modal_total_cn = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
  by = sample]

# Sample pool, deduped to one row per patient (highest purity)
samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% c("01", "03") &
  cohort %in% TCGA_PROJECTS,
  .(sample, patient, cohort, purity)
]
samples_in <- merge(samples_in, mtap_cn, by = "sample", all.x = TRUE)
samples_in <- samples_in[!is.na(modal_total_cn)]
samples_in[, mtap_homdel := as.integer(modal_total_cn == 0)]
setorder(samples_in, patient, -purity)
samples_in <- samples_in[, .SD[1L], by = patient]

# Hypermutator exclusion
maf_ns <- maf[nonsilent == TRUE]
ns_per_patient <- maf_ns[, .(n_ns = .N), by = patient]
hypermut <- ns_per_patient[n_ns > HYPERMUT_NS_CUTOFF, patient]
samples_in <- samples_in[!patient %in% hypermut]
message(glue("[filter] samples after dedup+hypermut: {nrow(samples_in)}"))

# Patient × driver-gene mutation table
mut_long <- unique(maf_ns[patient %in% samples_in$patient &
                          hugo_symbol %in% CANCER_DRIVERS,
                          .(patient, gene = hugo_symbol)])
message(glue("[mut] unique (patient,driver) pairs: {nrow(mut_long)}"))

# ---------------------------------------------------------------------------
# Per (cohort, gene) tabulation
# ---------------------------------------------------------------------------
cohorts <- intersect(TCGA_PROJECTS, unique(samples_in$cohort))
out_list <- list()
for (co in cohorts) {
  pts_all    <- samples_in[cohort == co]
  pts_homdel <- pts_all[mtap_homdel == 1]
  pts_intact <- pts_all[mtap_homdel == 0]    # MTAP CN ≥ 1
  n_co       <- nrow(pts_all)
  n_homdel   <- nrow(pts_homdel)
  n_intact   <- nrow(pts_intact)
  if (n_co < MIN_COHORT_N || n_intact < 10) next

  # Baseline = MTAP-INTACT patients only (avoids comparing homdel against
  # itself in cohorts where homdel is a large share of all patients).
  mut_in_intact <- mut_long[patient %in% pts_intact$patient]
  gene_freq <- mut_in_intact[, .(n_mut_baseline = uniqueN(patient)), by = gene
                ][, baseline_rate := 100 * n_mut_baseline / n_intact
                ][baseline_rate >= 100 * MIN_GENE_FREQ_BASELINE]
  if (nrow(gene_freq) == 0) next

  # MTAP-homdel rates for those genes
  mut_in_homdel <- mut_long[patient %in% pts_homdel$patient]
  homdel_freq   <- mut_in_homdel[gene %in% gene_freq$gene,
                                  .(n_mut_homdel = uniqueN(patient)), by = gene]
  res <- merge(gene_freq, homdel_freq, by = "gene", all.x = TRUE)
  res[is.na(n_mut_homdel), n_mut_homdel := 0L]
  res[, `:=`(
    cohort               = co,
    n_cohort             = n_co,
    n_mtap_intact        = n_intact,
    n_mtap_homdel        = n_homdel,
    homdel_rate          = if (n_homdel > 0) 100 * n_mut_homdel / n_homdel else NA_real_
  )]
  res[, enrichment_ratio := homdel_rate / pmax(baseline_rate, 0.5)]
  out_list[[co]] <- res
}
result <- rbindlist(out_list, fill = TRUE)
out_pq <- fs::path(RESULTS_DIR, "03b_mutpop_long.parquet")
write_parquet(result, out_pq)

# ---------------------------------------------------------------------------
# Plot — per-cohort small-multiple lollipops; top N drivers per cohort
# ---------------------------------------------------------------------------
plot_cohorts <- result[n_mtap_homdel >= MIN_HOMDEL_FOR_PANEL, unique(cohort)]
message(glue("[plot] cohorts with ≥{MIN_HOMDEL_FOR_PANEL} MTAP-homdel patients: {length(plot_cohorts)}"))

plot_df <- result[cohort %in% plot_cohorts]
# Top N genes per cohort by MTAP-homdel mutation rate
setorder(plot_df, cohort, -homdel_rate)
plot_df <- plot_df[, head(.SD, TOP_N_PER_COHORT), by = cohort]

# Cohort order = Stage 1 homdel% desc (for visual continuity)
s1 <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
co_order <- intersect(s1[order(-homdel_pct)]$cohort, plot_cohorts)
plot_df[, cohort := factor(cohort, levels = co_order)]

# Panel labels
panel_meta <- unique(plot_df[, .(cohort, n_cohort, n_mtap_homdel)])
panel_meta[, panel_label := paste0(sub("^TCGA-", "", cohort),
                                    "   CN0=", n_mtap_homdel,
                                    " / n=", n_cohort)]
plot_df <- merge(plot_df, panel_meta[, .(cohort, panel_label)], by = "cohort")
plot_df[, panel_label := factor(panel_label,
                                levels = panel_meta[order(match(cohort, co_order))]$panel_label)]

# Long format for lollipop: one row per (cohort, gene, type)
long <- rbind(
  plot_df[, .(panel_label, cohort, gene, rate = homdel_rate,   type = "MTAP-homdel pts")],
  plot_df[, .(panel_label, cohort, gene, rate = baseline_rate, type = "MTAP-intact baseline")]
)
long[, type := factor(type, levels = c("MTAP-intact baseline", "MTAP-homdel pts"))]

# Order genes within each panel by homdel_rate (so top gene appears at top)
gene_order <- plot_df[, .(panel_label, gene, ord = homdel_rate)]
gene_order[, gene_f := factor(gene, levels = rev(unique(gene[order(ord)]))), by = panel_label]
long <- merge(long, gene_order[, .(panel_label, gene, gene_f)], by = c("panel_label", "gene"))

p <- ggplot(long, aes(x = rate, y = gene_f)) +
  geom_line(aes(group = gene), color = "grey60", linewidth = 0.5) +
  geom_point(aes(color = type, fill = type, shape = type), size = 2.6, stroke = 0.5) +
  facet_wrap(~ panel_label, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("MTAP-intact baseline" = "#888780",
                                "MTAP-homdel pts"      = "#0F6E56"),
                     name = NULL) +
  scale_fill_manual(values  = c("MTAP-intact baseline" = "#FFFFFF",
                                "MTAP-homdel pts"      = "#0F6E56"),
                    name = NULL) +
  scale_shape_manual(values = c("MTAP-intact baseline" = 21,
                                "MTAP-homdel pts"      = 19),
                     name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title    = "Mutational landscape of MTAP-homdel patients — by TCGA cohort",
    subtitle = wrap_text(glue("Top {TOP_N_PER_COHORT} cohort drivers (OncoKB; ≥ {round(100*MIN_GENE_FREQ_BASELINE)}% in MTAP-intact subset, length-bias blocklist applied; hypermutators > {HYPERMUT_NS_CUTOFF} non-silent SNVs excluded). Filled dark dot = mutation rate in MTAP-homdel patients; hollow grey = MTAP-intact baseline (CN ≥ 1, same cohort). Gap between dots = enrichment in the MTAP-homdel population."),
                          width = 150),
    x = "Mutation rate (%)",
    y = NULL,
    caption = wrap_text(local({
      homdel_per_co <- unique(result[, .(cohort, n_mtap_homdel)])
      excl <- homdel_per_co[n_mtap_homdel < MIN_HOMDEL_FOR_PANEL]
      glue("Cohorts shown: ≥ {MIN_HOMDEL_FOR_PANEL} MTAP-homdel patients (≥ 20 % stable proportion step). Excluded ({nrow(excl)} cohorts, too few homdel events): ",
           paste0(sub("^TCGA-", "", excl$cohort), " (n=", excl$n_mtap_homdel, ")",
                  collapse = ", "))
    }), width = 165)
  ) +
  theme_target_intel(base_size = 10) +
  theme(
    strip.text       = element_text(size = 9, face = "bold", color = "#1a1a18"),
    axis.text.y      = element_text(family = "mono", size = 8),
    panel.spacing    = unit(10, "pt"),
    legend.position  = "top"
  )

out_png <- fs::path(RESULTS_DIR, "03b_mutpop_lollipop.png")
n_panels <- length(plot_cohorts)
ggsave(out_png, p,
       width  = 15,
       height = ceiling(n_panels / 3) * 2.7 + 2.6, dpi = 160)
message(glue("[plot] {out_png}"))

# Audit
write_audit(
  audit_row(
    analysis    = "03b_mutpop",
    source      = "ABSOLUTE PanCanAtlas + MC3 v0.2.8 PUBLIC + OncoKB drivers",
    n_in        = nrow(samples_in),
    n_excluded  = length(hypermut),
    test        = "descriptive rates (no inferential test)",
    adjust      = "none",
    output_path = paste(out_pq, out_png, sep = "; "),
    notes       = glue("cohorts_plotted={length(plot_cohorts)}; top_n_per_cohort={TOP_N_PER_COHORT}; baseline_min={MIN_GENE_FREQ_BASELINE}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 3B complete.")
