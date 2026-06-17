## Stage 3A — CN co-deletion partners of MTAP, per TCGA cohort.
## "Among MTAP-homdel patients, what other genes are also deleted?"

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

MIN_HOMDEL_FOR_PANEL <- 5L      # min MTAP-homdel patients for stable cohort %

# ---------------------------------------------------------------------------
# Partner gene loci (hg19 / GRCh37 — matches ABSOLUTE segments)
# ---------------------------------------------------------------------------
# Three groups:
#   focal_9p21  — within ~3 Mb of MTAP, almost always lost together with MTAP
#   arm_9p      — same chromosome arm but >5 Mb away (needs arm-level del)
#   distant_TSG — other chromosomes, grouped by chr (reveals arm-co-loss patterns)
PARTNER_LOCI <- rbind(
  # Focal 9p21 (close to MTAP)
  data.frame(gene="IFNB1",  chrom="9",  start=21077104,  end=21077945, group="focal_9p21"),
  data.frame(gene="IFNW1",  chrom="9",  start=21180594,  end=21181182, group="focal_9p21"),
  data.frame(gene="IFNA1",  chrom="9",  start=21440396,  end=21440961, group="focal_9p21"),
  # MTAP itself (21802635-21865969) — excluded from display (always 100%)
  data.frame(gene="CDKN2A", chrom="9",  start=21967751,  end=21994490, group="focal_9p21"),
  data.frame(gene="CDKN2B", chrom="9",  start=22002902,  end=22009362, group="focal_9p21"),
  data.frame(gene="ELAVL2", chrom="9",  start=23690103,  end=23826138, group="focal_9p21"),
  # 9p arm-level (further away on chr9)
  data.frame(gene="MLLT3",  chrom="9",  start=20341866,  end=20622492, group="arm_9p"),
  data.frame(gene="PTPRD",  chrom="9",  start=8314246,   end=10612723, group="arm_9p"),
  # Distant tumour suppressors (grouped by chromosome)
  data.frame(gene="PTEN",   chrom="10", start=89623194,  end=89728532,  group="distant_TSG"),
  data.frame(gene="ATM",    chrom="11", start=108093558, end=108239826, group="distant_TSG"),
  data.frame(gene="BRCA2",  chrom="13", start=32890598,  end=32973805,  group="distant_TSG"),
  data.frame(gene="RB1",    chrom="13", start=48877887,  end=49056026,  group="distant_TSG"),
  data.frame(gene="TP53",   chrom="17", start=7571720,   end=7590868,   group="distant_TSG"),
  data.frame(gene="NF1",    chrom="17", start=29421945,  end=29704695,  group="distant_TSG"),
  data.frame(gene="BRCA1",  chrom="17", start=41196312,  end=41277500,  group="distant_TSG"),
  data.frame(gene="SMAD4",  chrom="18", start=48494427,  end=48611415,  group="distant_TSG"),
  data.frame(gene="STK11",  chrom="19", start=1205798,   end=1228434,   group="distant_TSG"),
  # MTAP self — kept for QC, dropped from plot
  data.frame(gene="MTAP",   chrom="9",  start=21802635,  end=21865969, group="self_sanity"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
seg <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_segments.parquet")))
pur <- as.data.table(read_parquet(fs::path(CACHE_DIR, "absolute_samples.parquet")))
setnames(seg, tolower(names(seg)))

# Stage-1-equivalent sample pool, deduped to one row per patient (highest purity).
samples_in <- pur[
  qc_pass == TRUE &
  sample_type %in% c("01", "03") &
  cohort %in% TCGA_PROJECTS,
  .(sample, patient, cohort, purity)
]
setorder(samples_in, patient, -purity)
samples_in <- samples_in[, .SD[1L], by = patient]
message(glue("[filter] samples: {nrow(samples_in)}"))

# ---------------------------------------------------------------------------
# Per-sample CN for every partner gene (overlap-weighted, like Stages 1/2)
# ---------------------------------------------------------------------------
gene_cn <- function(g, ch, s, e) {
  hit <- seg[as.character(chromosome) == ch & start <= e & end >= s]
  if (nrow(hit) == 0) return(data.table())
  hit[, overlap := pmin(end, e) - pmax(start, s) + 1
     ][, .(gene = g,
           cn   = round(weighted.mean(modal_total_cn, overlap, na.rm = TRUE))),
       by = sample]
}

all_calls <- rbindlist(lapply(seq_len(nrow(PARTNER_LOCI)), function(i) {
  L <- PARTNER_LOCI[i, ]
  gene_cn(L$gene, L$chrom, L$start, L$end)
}))

# Attach cohort
all_calls <- merge(all_calls, samples_in[, .(sample, patient, cohort)],
                   by = "sample")
message(glue("[cn] partner-gene CN calls: {nrow(all_calls)}; samples covered: {uniqueN(all_calls$sample)}"))

# ---------------------------------------------------------------------------
# Identify MTAP-homdel patient set per cohort
# ---------------------------------------------------------------------------
mtap_calls <- all_calls[gene == "MTAP"]
mtap_homdel_pts <- mtap_calls[cn == 0, .(patient, cohort)]
mtap_homdel_pts_by_cohort <- mtap_homdel_pts[, .N, by = cohort][order(-N)]
message("[mtap] homdel patients per cohort:"); print(mtap_homdel_pts_by_cohort)

# ---------------------------------------------------------------------------
# Per (cohort, partner) tabulation among MTAP-homdel patients
# ---------------------------------------------------------------------------
calls_in_homdel <- merge(all_calls, mtap_homdel_pts, by = c("patient", "cohort"))
per_partner <- calls_in_homdel[, .(
  partner_n_homdel  = sum(cn == 0, na.rm = TRUE),
  partner_n_anyloss = sum(cn <= 1, na.rm = TRUE),
  n_mtap_homdel     = .N
), by = .(cohort, gene)]
per_partner[, `:=`(
  partner_pct_homdel  = 100 * partner_n_homdel  / n_mtap_homdel,
  partner_pct_anyloss = 100 * partner_n_anyloss / n_mtap_homdel
)]

# Baseline anyloss% computed on MTAP-INTACT patients only (CN ≥ 1).
# Using "all cohort" as baseline would partially compare MTAP-homdel against
# itself, biasing enrichment downward — especially in GBM/MESO where homdel
# is ~30-45% of the cohort.
intact_pts <- mtap_calls[cn >= 1, .(patient, cohort)]
calls_in_intact <- merge(all_calls, intact_pts, by = c("patient", "cohort"))
baseline <- calls_in_intact[, .(
  baseline_n         = .N,
  baseline_n_anyloss = sum(cn <= 1, na.rm = TRUE)
), by = .(cohort, gene)]
baseline[, baseline_pct_anyloss := 100 * baseline_n_anyloss / baseline_n]

result <- merge(per_partner, baseline[, .(cohort, gene, baseline_pct_anyloss)],
                by = c("cohort", "gene"))

# Restrict to cohorts with enough MTAP-homdel patients to be informative
keep_cohorts <- mtap_homdel_pts_by_cohort[N >= MIN_HOMDEL_FOR_PANEL]$cohort
result <- result[cohort %in% keep_cohorts]
message(glue("[filter] cohorts with ≥{MIN_HOMDEL_FOR_PANEL} MTAP-homdel patients: {length(keep_cohorts)}"))

# Attach gene group
result <- merge(result,
                as.data.table(PARTNER_LOCI)[, .(gene, group)],
                by = "gene")

result[, enrichment_pp := partner_pct_anyloss - baseline_pct_anyloss]
out_pq <- fs::path(RESULTS_DIR, "03a_codeletion_partners.parquet")
write_parquet(result, out_pq)

# ---------------------------------------------------------------------------
# Plot — heatmap of ENRICHMENT (Δ percentage points) coloured;
# raw rate as label. MTAP self-row dropped.
# ---------------------------------------------------------------------------
plot_df <- result[gene != "MTAP"]
plot_df <- plot_df[!is.na(enrichment_pp)]

# Cohort order by Stage 1 homdel%
s1 <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
cohort_order <- intersect(s1[order(-homdel_pct)]$cohort, unique(plot_df$cohort))

# Gene order: focal_9p21 (by hg19 pos) → arm_9p (by hg19 pos) → distant_TSG (by chrom then pos)
focal_order   <- as.data.table(PARTNER_LOCI)[group == "focal_9p21"][order(start)]$gene
arm_order     <- as.data.table(PARTNER_LOCI)[group == "arm_9p"][order(start)]$gene
distant_dt    <- as.data.table(PARTNER_LOCI)[group == "distant_TSG"]
distant_dt[, chrom_n := as.integer(chrom)]
distant_order <- distant_dt[order(chrom_n, start)]$gene

# rev() so the first-listed gene appears at TOP of y-axis
gene_levels <- rev(c(focal_order, arm_order, distant_order))

plot_df[, cohort := factor(cohort, levels = cohort_order)]
plot_df[, gene   := factor(gene,   levels = gene_levels)]

# x-axis labels: cohort + n_mtap_homdel
xlab_map <- setNames(
  paste0(sub("^TCGA-", "", mtap_homdel_pts_by_cohort$cohort),
         "\nCN0=", mtap_homdel_pts_by_cohort$N),
  mtap_homdel_pts_by_cohort$cohort
)
xlab_vec <- xlab_map[as.character(levels(plot_df$cohort))]

# Cell label: raw anyloss% (and homdel% only when meaningfully different)
plot_df[, cell_label := fifelse(
  abs(partner_pct_anyloss - partner_pct_homdel) >= 10,
  sprintf("%.0f\n(%.0f hom)", partner_pct_anyloss, partner_pct_homdel),
  sprintf("%.0f", partner_pct_anyloss)
)]

# Horizontal separators between gene groups (in display order)
n_distant <- length(distant_order)
n_arm     <- length(arm_order)
sep_y     <- c(n_distant + 0.5,                 # between distant ↑ and arm_9p ↑
               n_distant + n_arm + 0.5)         # between arm_9p ↑ and focal_9p21 ↑

p <- ggplot(plot_df, aes(x = cohort, y = gene, fill = enrichment_pp)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = cell_label,
                color = abs(enrichment_pp) > 35),
            size = 2.5, lineheight = 0.85, show.legend = FALSE) +
  scale_color_manual(values = c(`FALSE` = "#1a1a18", `TRUE` = "#FFFFFF")) +
  scale_fill_gradient2(
    low      = "#A32D2D", mid = "#FFFFFF", high = "#185FA5",
    midpoint = 0,
    limits   = c(-60, 60), oob = scales::squish,
    breaks   = c(-60, -30, 0, 30, 60),
    labels   = c("≤ −60\n(under)", "−30", "0\n(=baseline)", "+30", "≥ +60\n(enriched)"),
    name     = "Δ anyloss%\n(MTAP-homdel\nvs cohort baseline)"
  ) +
  scale_x_discrete(labels = xlab_vec, drop = FALSE) +
  geom_hline(yintercept = sep_y, color = "grey50", linewidth = 0.6, linetype = "dashed") +
  labs(
    title    = "Co-deletion partners of MTAP — per TCGA cohort",
    subtitle = wrap_text(glue("Cell colour = enrichment (Δ percentage points) of anyloss (CN ≤ 1) at partner gene in MTAP-homdel vs MTAP-intact (CN ≥ 1) patients. Cell label = raw anyloss% in MTAP-homdel patients (and homdel% if ≥ 10 pts apart). Cohorts sorted by Stage 1 homdel%. Gene groups (top → bottom): focal 9p21 | 9p arm-level | distant TSGs. High PTPRD / MLLT3 co-loss in many cohorts indicates the deletion footprint is often arm-level, not strictly focal 9p21.3."),
                          width = 150),
    x = NULL, y = NULL,
    caption = wrap_text(local({
      excl <- mtap_homdel_pts_by_cohort[N < MIN_HOMDEL_FOR_PANEL]
      glue("Cohorts shown: ≥ {MIN_HOMDEL_FOR_PANEL} MTAP-homdel patients (≥ 20 % stable proportion step). Excluded ({nrow(excl)} cohorts, too few homdel events): ",
           paste0(sub("^TCGA-", "", excl$cohort), " (n=", excl$N, ")", collapse = ", "))
    }), width = 165)
  ) +
  theme_target_intel(base_size = 10) +
  theme(
    panel.grid        = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, family = "mono", size = 8,
                                     lineheight = 0.85),
    axis.text.y       = element_text(family = "mono", size = 9),
    legend.key.height = unit(22, "pt")
  )

out_png <- fs::path(RESULTS_DIR, "03a_codeletion_partners.png")
ggsave(out_png, p, width = max(12, 0.62 * length(cohort_order) + 5),
       height = 0.45 * length(gene_levels) + 3.4, dpi = 160)
message(glue("[plot] {out_png}"))

# Audit
write_audit(
  audit_row(
    analysis    = "03a_codeletion",
    source      = "ABSOLUTE PanCanAtlas (Taylor 2018)",
    n_in        = nrow(samples_in),
    n_excluded  = 0L,
    test        = "descriptive proportions",
    adjust      = "none",
    output_path = paste(out_pq, out_png, sep = "; "),
    notes       = glue("partner_genes={nrow(PARTNER_LOCI)}; cohorts_plotted={length(keep_cohorts)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 3A complete.")
