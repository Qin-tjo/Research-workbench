## Panel 5 — final dashboard renderer.
## Composes all Stages + Panels into a single sticky-nav HTML matching
## the original PRMT5 reference layout.

suppressPackageStartupMessages({
  library(yaml)
  library(arrow)
  library(data.table)
  library(glue)
  library(fs)
  library(htmltools)
  library(base64enc)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))

DATA_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"
cit  <- yaml::read_yaml(fs::path(DATA_DIR, "citations.yaml"))
p2   <- yaml::read_yaml(fs::path(DATA_DIR, "panel2_mechanism.yaml"))
p3d  <- yaml::read_yaml(fs::path(DATA_DIR, "panel3_drugs.yaml"))$drugs
p3r  <- yaml::read_yaml(fs::path(DATA_DIR, "panel3_results.yaml"))$readouts
p4   <- yaml::read_yaml(fs::path(DATA_DIR, "panel4_synthesis.yaml"))

# Parquets
s1     <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "01_mtap_deletion_freq.parquet")))
s2     <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "02_cn_expression_per_cohort.parquet")))
s3a    <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "03a_codeletion_partners.parquet")))
s3b    <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "03b_mutpop_long.parquet")))
s4_per <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "04_focality_per_cohort.parquet")))
s5     <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "05_msk_validation.parquet")))
audit  <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "audit.parquet")))
trials <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "06_clinical_trials_enriched.parquet")))
rollup <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "06_clinical_summary.parquet")))

# Re-derive normalised trial status / phase
status_map <- c(
  RECRUITING="Active", ACTIVE_NOT_RECRUITING="Active",
  ENROLLING_BY_INVITATION="Active", NOT_YET_RECRUITING="Planned",
  COMPLETED="Completed", TERMINATED="Terminated",
  WITHDRAWN="Terminated", SUSPENDED="Terminated"
)
trials[, status_short := factor(unname(status_map[overall_status]),
                                levels = c("Active","Planned","Completed","Terminated"))]
trials[, phase_short := fcase(
  phase %in% c("EARLY_PHASE1","Ph1"), "Ph1",
  phase == "Ph1/Ph2",                "Ph1/2",
  phase == "Ph2",                    "Ph2",
  phase == "Ph2/Ph3",                "Ph2/3",
  phase == "Ph3",                    "Ph3",
  default = phase
)]
trials[, phase_short := factor(phase_short, levels = c("Ph1","Ph1/2","Ph2","Ph2/3","Ph3"))]

# Embed PNGs inline as base64 (single-file deliverable)
embed_img <- function(filename, alt = "", cls = "") {
  p <- fs::path(RESULTS_DIR, filename)
  if (!file_exists(p)) return(sprintf('<div class="missing-img">[missing: %s]</div>', filename))
  b64 <- base64enc::base64encode(p)
  sprintf('<img class="%s" alt="%s" src="data:image/png;base64,%s">',
          cls, htmltools::htmlEscape(alt), b64)
}

esc <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("")
  htmltools::htmlEscape(as.character(x))
}
nz <- function(x) !is.null(x) && length(x) >= 1 && nzchar(trimws(as.character(x)[1]))

cite_chip <- function(key) {
  c <- cit[[key]]
  if (is.null(c)) return(sprintf('<span class="cite missing">[%s?]</span>', esc(key)))
  short <- sub(",.*", "", c$authors)
  # Citation chip links straight to the external publication (PubMed / DOI /
  # journal page). The bibliography in Section R provides the full audit
  # trail with the same URLs; reference-list anchor links are still
  # available via the ref-XXX ids if needed.
  url <- if (!is.null(c$url) && nzchar(c$url)) c$url else paste0("#ref-", key)
  sprintf('<a class="cite" href="%s" target="_blank" rel="noopener" title="%s">%s %d</a>',
          esc(url), esc(c$title), esc(short), c$year)
}
cite_html <- function(keys) {
  if (is.null(keys) || length(keys) == 0) return("")
  parts <- vapply(keys, cite_chip, character(1))
  paste0('<sup class="cites">', paste(parts, collapse = " "), '</sup>')
}

# Collect every citation key used anywhere → ordered de-dup for reference list
collect_keys <- function() {
  keys <- c()
  # Section 1 hard-coded inline citations
  keys <- c(keys, "taylor2018", "cerami2012", "kryukov2016", "wilks2021",
                  "zhang2018", "ellrott2018", "chakravarty2017")
  for (s in p2$flow)          keys <- c(keys, s$citations)
  for (b in p2$biomarker)     keys <- c(keys, b$citations)
  for (s in p2$strategies)    keys <- c(keys, s$citations)
  for (u in p2$uncertainties) keys <- c(keys, if (is.list(u)) u$citations else NULL)
  for (d in p3d) if (nz(d$primary_publication)) keys <- c(keys, d$primary_publication)
  for (r in p3r) {
    keys <- c(keys, r$citation)
    keys <- c(keys, r$additional_citations)
  }
  keys <- c(keys, p4$thesis$citations)
  for (p in p4$patient_selection) {
    keys <- c(keys, if (is.list(p$refs)) p$refs$citations else NULL,
              p$citations)
  }
  for (co in p4$combinations) {
    keys <- c(keys, if (is.list(co$refs)) co$refs$citations else NULL,
              co$citations)
  }
  for (r in p4$risks) {
    keys <- c(keys, if (is.list(r$refs)) r$refs$citations else NULL,
              r$citations)
  }
  for (s in p4$next_steps) keys <- c(keys, s$citations)
  unique(unlist(keys))
}

REF_KEYS <- collect_keys()

# ============================================================================
# Section 1 — Genomic landscape
# ============================================================================
# ---------------------------------------------------------------------------
# Interactive Stage 1 SVG — bars with hover tooltips + scroll-triggered grow-in
# ---------------------------------------------------------------------------
make_stage1_svg <- function(dt) {
  rows <- dt[order(-homdel_pct, -hetdel_pct)]
  n <- nrow(rows)
  row_h    <- 22L
  row_gap  <- 4L
  pad_top  <- 44L
  pad_bot  <- 38L
  pad_left <- 116L
  pad_right<- 246L
  bar_w    <- 450L
  width    <- pad_left + bar_w + pad_right
  height   <- pad_top + n * (row_h + row_gap) + pad_bot
  max_pct  <- 90       # x scale upper bound (in %)
  ticks    <- c(0, 30, 60, 90)

  # Axis ticks
  tick_lines <- vapply(ticks, function(t) {
    x <- pad_left + (t / max_pct) * bar_w
    sprintf('<line class="axis-tick" x1="%.1f" y1="%d" x2="%.1f" y2="%d"></line><text class="axis-label" x="%.1f" y="%d">%d%%</text>',
            x, pad_top - 4, x, height - pad_bot + 6,
            x, height - pad_bot + 22, t)
  }, character(1))

  # Rows
  row_blocks <- vapply(seq_len(n), function(i) {
    r  <- rows[i]
    y  <- pad_top + (i - 1) * (row_h + row_gap)
    homw <- (r$homdel_pct / max_pct) * bar_w
    hetw <- (r$hetdel_pct / max_pct) * bar_w
    totw <- homw + hetw
    cohort <- sub("^TCGA-", "", r$cohort)
    # Bars rendered at their final width/position so the figure is readable
    # without JavaScript (e.g. iOS Files preview, email clients).  JS may
    # still animate the bars in via CSS transitions, but the defaults are
    # the real values, not zero.
    sprintf('<g class="bar-row" data-cohort="%s" data-n="%d" data-homdel="%.1f" data-hetdel="%.1f" data-total="%.1f">
      <rect class="row-hit" x="0" y="%d" width="%d" height="%d"></rect>
      <text class="cohort-label" x="%d" y="%d">%s</text>
      <rect class="bar bar-hom" x="%d" y="%d" rx="2" ry="2" height="%d" width="%.2f"></rect>
      <rect class="bar bar-het" x="%.2f" y="%d" rx="2" ry="2" height="%d" width="%.2f"></rect>
      <text class="row-pct" x="%.1f" y="%d">n=%d · %.1f%% / %.1f%%</text>
    </g>',
      cohort, r$n_cohort, r$homdel_pct, r$hetdel_pct, r$homdel_pct + r$hetdel_pct,
      y - 2, width, row_h + 4,
      pad_left - 8, y + row_h / 2 + 4, cohort,
      pad_left, y, row_h, homw,
      pad_left + homw, y, row_h, hetw,
      pad_left + totw + 6, y + row_h / 2 + 4,
      r$n_cohort, r$homdel_pct, r$hetdel_pct
    )
  }, character(1))

  legend_y <- pad_top - 22
  sprintf('
<svg class="stage1-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet"
     xmlns="http://www.w3.org/2000/svg" role="img"
     aria-label="MTAP deletion frequency across 33 TCGA cohorts">

  <g class="legend">
    <rect x="%d" y="%d" width="11" height="11" rx="2" class="bar-hom"></rect>
    <text x="%d" y="%d" class="legend-text">Homozygous deletion (CN=0)</text>
    <rect x="%d" y="%d" width="11" height="11" rx="2" class="bar-het"></rect>
    <text x="%d" y="%d" class="legend-text">Heterozygous deletion (CN=1)</text>
  </g>

  <line class="axis-baseline" x1="%d" y1="%d" x2="%d" y2="%d"></line>
  %s

  %s

  <text class="axis-title" x="%d" y="%d" text-anchor="middle">%% of cohort samples</text>
</svg>',
    width, height,
    pad_left, legend_y, pad_left + 16, legend_y + 9,
    pad_left + 195, legend_y, pad_left + 211, legend_y + 9,
    pad_left, height - pad_bot, pad_left + bar_w, height - pad_bot,
    paste(tick_lines, collapse = ""),
    paste(row_blocks, collapse = ""),
    pad_left + bar_w / 2, height - pad_bot + 32
  )
}

s1_svg <- make_stage1_svg(s1)

# ---------------------------------------------------------------------------
# Figure 2 — CN bin vs log2(TPM+1) median trajectories (small multiples)
# ---------------------------------------------------------------------------
make_figure2_svg <- function(dt) {
  rows <- dt[order(-spearman_rho, na.last = TRUE)]
  n   <- nrow(rows)
  # Tuned for side-by-side rendering with Figure 1 (so 4 cols, slightly taller panels)
  ncol <- 4L
  nrowg <- ceiling(n / ncol)
  panel_w <- 150; panel_h <- 105; panel_gap_x <- 14; panel_gap_y <- 24
  pad_left <- 48; pad_top <- 64; pad_bot <- 40
  width  <- pad_left + ncol * panel_w + (ncol - 1) * panel_gap_x + 16
  height <- pad_top + nrowg * panel_h + (nrowg - 1) * panel_gap_y + pad_bot

  # Y-domain from all medians
  y_vals <- unlist(rows[, .(median_cn0, median_cn1, median_cn2, median_cn3p)])
  y_vals <- y_vals[is.finite(y_vals)]
  y_min  <- max(0, floor(min(y_vals, na.rm = TRUE)))
  y_max  <- ceiling(max(y_vals, na.rm = TRUE) + 0.2)
  x_lbls <- c("0", "1", "2", "3+")
  bin_meds <- c("median_cn0","median_cn1","median_cn2","median_cn3p")
  bin_ns   <- c("n_cn0","n_cn1","n_cn2","n_cn3plus")

  to_x <- function(i, panel_x0) panel_x0 + 18 + (i - 1) * ((panel_w - 30) / 3)
  to_y <- function(v, panel_y0) panel_y0 + panel_h - 24 -
                                  ((v - y_min) / (y_max - y_min)) * (panel_h - 38)

  panel_blocks <- vapply(seq_len(n), function(k) {
    r <- rows[k]
    col <- ((k - 1) %% ncol)
    rr  <- ((k - 1) %/% ncol)
    px <- pad_left + col * (panel_w + panel_gap_x)
    py <- pad_top  + rr  * (panel_h + panel_gap_y)
    meds <- as.numeric(r[, ..bin_meds])
    ns   <- as.integer(r[, ..bin_ns])
    rho_s  <- if (is.na(r$spearman_rho)) "NA" else sprintf("%.2f", r$spearman_rho)

    # Connecting line through valid points only
    pts_xy <- vapply(seq_along(meds), function(i) {
      if (is.na(meds[i])) NA_character_ else sprintf("%.1f,%.1f", to_x(i, px), to_y(meds[i], py))
    }, character(1))
    pts_xy <- pts_xy[!is.na(pts_xy)]
    line_path <- if (length(pts_xy) >= 2)
      sprintf('<polyline class="f2-line" points="%s"></polyline>',
              paste(pts_xy, collapse = " ")) else ""

    # Dot per bin with size scaled by n
    n_max <- max(ns, na.rm = TRUE)
    dots <- vapply(seq_along(meds), function(i) {
      if (is.na(meds[i])) return("")
      r_dot <- if (is.na(ns[i]) || n_max == 0) 2 else 2 + (ns[i] / n_max) * 4
      sprintf('<circle class="f2-dot f2-cn%d" cx="%.1f" cy="%.1f" r="%.1f"></circle>',
              i - 1, to_x(i, px), to_y(meds[i], py), r_dot)
    }, character(1))

    # Per-bin tick labels at bottom
    xtick <- vapply(seq_along(x_lbls), function(i)
      sprintf('<text class="f2-xtick" x="%.1f" y="%.1f">%s</text>',
              to_x(i, px), py + panel_h - 8, x_lbls[i]), character(1))

    cohort_short <- sub("^TCGA-", "", r$cohort)
    meds_str <- vapply(seq_along(meds), function(i)
      if (is.na(meds[i])) "—" else sprintf("%.1f", meds[i]), character(1))
    n_str    <- vapply(seq_along(ns), function(i)
      if (is.na(ns[i])) "—" else as.character(ns[i]), character(1))

    sprintf('<g class="f2-panel" data-cohort="%s" data-n="%d" data-rho="%s" data-cn0n="%s" data-cn1n="%s" data-cn2n="%s" data-cn3n="%s" data-cn0m="%s" data-cn1m="%s" data-cn2m="%s" data-cn3m="%s">
      <rect class="f2-bg" x="%d" y="%d" width="%d" height="%d" rx="5" ry="5"></rect>
      <text class="f2-title" x="%.1f" y="%.1f">%s · ρ=%s · n=%d</text>
      <line class="f2-axis" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"></line>
      %s %s %s
    </g>',
      cohort_short, r$n, rho_s,
      n_str[1], n_str[2], n_str[3], n_str[4],
      meds_str[1], meds_str[2], meds_str[3], meds_str[4],
      px, py, panel_w, panel_h,
      px + panel_w / 2, py + 14, cohort_short, rho_s, r$n,
      px + 12, py + panel_h - 22, px + panel_w - 6, py + panel_h - 22,
      line_path, paste(dots, collapse = ""), paste(xtick, collapse = "")
    )
  }, character(1))

  sprintf('<svg class="figure-svg fig2-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="MTAP CN vs expression per TCGA cohort">
    <text class="fig-axis-y" x="14" y="%.0f" transform="rotate(-90 14 %.0f)">log2(TPM+1) median</text>
    <text class="fig-axis-x" x="%.0f" y="%d" text-anchor="middle">MTAP allelic CN (ABSOLUTE)</text>
    <text class="fig-legend-note" x="%d" y="32">Dot size ∝ n samples per CN bin. Hover any panel for medians and counts.</text>
    %s
  </svg>',
    width, height,
    height / 2 + 60, height / 2 + 60,
    width / 2, height - 8,
    pad_left,
    paste(panel_blocks, collapse = "")
  )
}
fig2_svg <- make_figure2_svg(s2)

# ---------------------------------------------------------------------------
# Figure 3 — Focality (Stage 4) stacked horizontal bars, two facets
# ---------------------------------------------------------------------------
make_figure3_svg <- function(dt) {
  MIN_N <- 5L
  pc <- dt[n_samples >= MIN_N]
  s1_order <- s1[order(-homdel_pct)]$cohort
  pc[, cohort_ord := match(cohort, s1_order)]
  pc <- pc[!is.na(cohort_ord)]
  setorder(pc, cohort_ord, mtap_class)

  cohorts <- unique(pc$cohort)
  n <- length(cohorts)
  row_h    <- 19L
  row_gap  <- 4L
  facet_gap<- 70L
  pad_top  <- 78L
  pad_bot  <- 40L
  pad_left <- 96L
  facet_w  <- 320L
  facet_label_w <- 100L    # right-margin label space per facet
  width    <- pad_left + 2 * facet_w + 2 * facet_label_w + facet_gap + 16
  height   <- pad_top + n * (row_h + row_gap) + pad_bot

  facets <- list(
    list(class = "hetdel",
         label = "Heterozygous deletion (CN=1)",
         x0 = pad_left),
    list(class = "homdel",
         label = "Homozygous deletion (CN=0)",
         x0 = pad_left + facet_w + facet_label_w + facet_gap)
  )

  row_blocks <- vapply(seq_along(cohorts), function(i) {
    co <- cohorts[i]
    co_short <- sub("^TCGA-", "", co)
    y <- pad_top + (i - 1) * (row_h + row_gap)
    parts <- vapply(facets, function(f) {
      r <- pc[cohort == co & mtap_class == f$class]
      if (nrow(r) == 0) return(sprintf(
        '<text class="f3-nodata" x="%.1f" y="%.1f">no data</text>',
        f$x0 + facet_w / 2, y + row_h / 2 + 4))
      fw <- (r$pct_focal       / 100) * facet_w
      iw <- (r$pct_intermediate/ 100) * facet_w
      aw <- (r$pct_arm         / 100) * facet_w
      label <- sprintf('n=%d · %.1f Mb', r$n_samples, r$median_mb)
      sprintf('<g class="f3-row" data-cohort="%s" data-class="%s" data-n="%d" data-med="%.2f" data-focal="%.1f" data-inter="%.1f" data-arm="%.1f">
        <rect class="f3-hit" x="%d" y="%d" width="%d" height="%d"></rect>
        <rect class="f3-seg f3-focal" x="%d"   y="%d" rx="2" ry="2" height="%d" width="%.2f"></rect>
        <rect class="f3-seg f3-inter" x="%.2f" y="%d" rx="2" ry="2" height="%d" width="%.2f"></rect>
        <rect class="f3-seg f3-arm"   x="%.2f" y="%d" rx="2" ry="2" height="%d" width="%.2f"></rect>
        <text class="f3-row-label" x="%.1f" y="%.1f">%s</text>
      </g>',
        co_short, f$class, r$n_samples, r$median_mb,
        r$pct_focal, r$pct_intermediate, r$pct_arm,
        f$x0 - 4, y - 2, facet_w + facet_label_w + 8, row_h + 4,
        f$x0,           y, row_h, fw,
        f$x0 + fw,      y, row_h, iw,
        f$x0 + fw + iw, y, row_h, aw,
        f$x0 + facet_w + 6, y + row_h / 2 + 4, label
      )
    }, character(1))

    sprintf('<g class="f3-cohort-row">
      <text class="cohort-label" x="%d" y="%.1f" text-anchor="end">%s</text>
      %s
    </g>',
      pad_left - 10, y + row_h / 2 + 4, co_short,
      paste(parts, collapse = ""))
  }, character(1))

  # Facet titles + axis tick lines (facet titles sit just above the bars,
  # legend lives in its own top row above the titles)
  facet_titles <- vapply(facets, function(f) {
    ticks <- vapply(c(0, 25, 50, 75, 100), function(t) {
      x <- f$x0 + (t / 100) * facet_w
      sprintf('<line class="axis-tick" x1="%.1f" y1="%d" x2="%.1f" y2="%d"></line><text class="axis-label" x="%.1f" y="%.1f">%d%%</text>',
              x, pad_top - 6, x, height - pad_bot + 4,
              x, height - pad_bot + 22, t)
    }, character(1))
    sprintf('<text class="f3-facet-title" x="%.1f" y="%d" text-anchor="middle">%s</text>%s',
            f$x0 + facet_w / 2, pad_top - 16, f$label,
            paste(ticks, collapse = ""))
  }, character(1))

  legend <- sprintf('<g class="f3-legend">
    <rect class="f3-focal" x="%d" y="14" width="11" height="11" rx="2"></rect>
    <text class="legend-text" x="%d" y="23">Focal (&lt; 3 Mb)</text>
    <rect class="f3-inter" x="%d" y="14" width="11" height="11" rx="2"></rect>
    <text class="legend-text" x="%d" y="23">Intermediate (3–25 Mb)</text>
    <rect class="f3-arm f3-arm-legend" x="%d" y="14" width="11" height="11" rx="2"></rect>
    <text class="legend-text" x="%d" y="23">Arm-level (≥ 25 Mb)</text>
  </g>',
      pad_left,        pad_left + 18,
      pad_left + 150,  pad_left + 168,
      pad_left + 340,  pad_left + 358)

  sprintf('<svg class="figure-svg fig3-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="MTAP deletion focality by cohort">
    %s
    %s
    %s
    <text class="fig-axis-x" x="%d" y="%d" text-anchor="middle">%% of deleted samples</text>
  </svg>',
    width, height,
    legend,
    paste(facet_titles, collapse = ""),
    paste(row_blocks, collapse = ""),
    width / 2, height - 6
  )
}
fig3_svg <- make_figure3_svg(s4_per)

# ---------------------------------------------------------------------------
# Figure 4 — co-deletion partners heatmap (Stage 3A)
# ---------------------------------------------------------------------------
make_figure4_svg <- function(dt) {
  # Reuse the same group / order logic Stage 3A used
  d <- copy(dt)[gene != "MTAP" & !is.na(enrichment_pp)]
  s1_order <- s1[order(-homdel_pct)]$cohort
  d[, cohort_ord := match(cohort, s1_order)]
  d <- d[!is.na(cohort_ord)]

  cohorts <- unique(d[order(cohort_ord)]$cohort)
  # Group genes
  focal_genes   <- c("IFNB1","IFNW1","IFNA1","CDKN2A","CDKN2B","ELAVL2")
  arm_genes     <- c("MLLT3","PTPRD")
  distant_genes <- c("PTEN","ATM","BRCA2","RB1","TP53","NF1","BRCA1","SMAD4","STK11")
  gene_order <- c(focal_genes, arm_genes, distant_genes)
  gene_order <- intersect(gene_order, unique(d$gene))

  n_co <- length(cohorts)
  n_ge <- length(gene_order)
  cell_w <- 40; cell_h <- 26
  pad_left <- 96; pad_top <- 52; pad_bot <- 100; pad_right <- 80
  width  <- pad_left + n_co * cell_w + pad_right
  height <- pad_top + n_ge * cell_h + pad_bot

  color_for <- function(e) {
    if (is.na(e)) return("#e8e6de")
    v <- pmax(-60, pmin(60, e))
    # Amber-600 (-60) — white (0) — Teal-800 (+60)
    # negative endpoint = #854F0B   (amber-600, "under-represented")
    # zero / midpoint  = #FFFFFF
    # positive endpoint = #085041   (teal-800, "enriched")
    if (v < 0) {
      t <- (0 - v) / 60
      r <- round(255 + (133 - 255) * t)
      g <- round(255 + (79  - 255) * t)
      b <- round(255 + (11  - 255) * t)
    } else {
      t <- v / 60
      r <- round(255 + (8   - 255) * t)
      g <- round(255 + (80  - 255) * t)
      b <- round(255 + (65  - 255) * t)
    }
    sprintf("rgb(%d,%d,%d)", r, g, b)
  }

  cells <- vapply(seq_along(cohorts), function(i) {
    vapply(seq_along(gene_order), function(j) {
      co <- cohorts[i]; ge <- gene_order[j]
      r <- d[cohort == co & gene == ge]
      x <- pad_left + (i - 1) * cell_w
      y <- pad_top  + (j - 1) * cell_h
      if (nrow(r) == 0) {
        return(sprintf('<rect class="f4-cell f4-na" x="%d" y="%d" width="%d" height="%d" data-cohort="%s" data-gene="%s"></rect>',
                       x, y, cell_w, cell_h, sub("^TCGA-", "", co), ge))
      }
      lbl <- if (abs(r$partner_pct_anyloss - r$partner_pct_homdel) >= 10)
              sprintf("%.0f", r$partner_pct_anyloss)
              else  sprintf("%.0f", r$partner_pct_anyloss)
      fontcol <- if (!is.na(r$enrichment_pp) && abs(r$enrichment_pp) > 35) "#ffffff" else "#1a1a18"
      sprintf('<g class="f4-cell-g" data-cohort="%s" data-gene="%s" data-anyloss="%.1f" data-homdel="%.1f" data-baseline="%.1f" data-enrich="%.1f" data-nmtap="%d">
        <rect class="f4-cell" x="%d" y="%d" width="%d" height="%d" rx="2" ry="2" fill="%s"></rect>
        <text class="f4-label" x="%.1f" y="%.1f" fill="%s">%s</text>
      </g>',
        sub("^TCGA-", "", co), ge,
        r$partner_pct_anyloss, r$partner_pct_homdel, r$baseline_pct_anyloss,
        r$enrichment_pp, r$n_mtap_homdel,
        x + 1, y + 1, cell_w - 2, cell_h - 2, color_for(r$enrichment_pp),
        x + cell_w / 2, y + cell_h / 2 + 3.5, fontcol, lbl)
    }, character(1))
  }, character(n_ge))

  # X-axis (cohort) labels: text-anchor="end" with -45° rotation makes the
  # label hang down-and-left from its column centre, with the upper-right
  # corner anchored just below the chart.
  x_label_y <- pad_top + n_ge * cell_h + 8
  x_labels <- vapply(seq_along(cohorts), function(i) {
    x <- pad_left + (i - 0.5) * cell_w
    co_short <- sub("^TCGA-", "", cohorts[i])
    sprintf('<text class="f4-xlab" x="%.1f" y="%d" text-anchor="end" transform="rotate(-45 %.1f %d)">%s</text>',
            x, x_label_y, x, x_label_y, co_short)
  }, character(1))

  y_labels <- vapply(seq_along(gene_order), function(j) {
    y <- pad_top + (j - 0.5) * cell_h + 4
    sprintf('<text class="f4-ylab" x="%d" y="%.1f">%s</text>', pad_left - 8, y, gene_order[j])
  }, character(1))

  # Group separators
  sep_y <- c(length(focal_genes), length(focal_genes) + length(arm_genes))
  sep_lines <- vapply(sep_y, function(s) {
    y <- pad_top + s * cell_h
    sprintf('<line class="f4-sep" x1="%d" y1="%.1f" x2="%d" y2="%.1f"></line>',
            pad_left, y, pad_left + n_co * cell_w, y)
  }, character(1))

  # Legend
  legend_x <- pad_left
  legend_y <- 26
  legend_grad <- '<defs>
    <linearGradient id="f4-grad" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0%"   stop-color="rgb(133,79,11)"></stop>
      <stop offset="50%"  stop-color="rgb(255,255,255)"></stop>
      <stop offset="100%" stop-color="rgb(8,80,65)"></stop>
    </linearGradient>
  </defs>'
  legend_svg <- sprintf('%s<rect x="%d" y="%d" width="200" height="10" fill="url(#f4-grad)" rx="2" ry="2" stroke="rgba(0,0,0,0.12)"></rect>
    <text class="legend-tick" x="%d" y="%d">−60</text>
    <text class="legend-tick" x="%d" y="%d">0</text>
    <text class="legend-tick" x="%d" y="%d">+60</text>
    <text class="legend-text" x="%d" y="%d">Δ anyloss%% (MTAP-homdel vs MTAP-intact)</text>',
    legend_grad,
    legend_x, legend_y,
    legend_x, legend_y - 4,
    legend_x + 95, legend_y - 4,
    legend_x + 188, legend_y - 4,
    legend_x + 215, legend_y + 8)

  sprintf('<svg class="figure-svg fig4-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="MTAP co-deletion partners heatmap">
    %s
    %s
    %s
    %s
    %s
  </svg>',
    width, height,
    legend_svg,
    paste(unlist(cells), collapse = ""),
    paste(y_labels, collapse = ""),
    paste(x_labels, collapse = ""),
    paste(sep_lines, collapse = "")
  )
}
fig4_svg <- make_figure4_svg(s3a)

# ---------------------------------------------------------------------------
# Figure 5 — mutational landscape of MTAP-homdel patients (Stage 3B lollipops)
# ---------------------------------------------------------------------------
make_figure5_svg <- function(dt) {
  MIN_N <- 5L
  d <- copy(dt)[n_mtap_homdel >= MIN_N]
  # Keep top 10 per cohort by homdel_rate
  setorder(d, cohort, -homdel_rate)
  d <- d[, head(.SD, 10L), by = cohort]
  s1_order <- s1[order(-homdel_pct)]$cohort
  cohorts <- intersect(s1_order, unique(d$cohort))

  ncols <- 3L
  nrowg <- ceiling(length(cohorts) / ncols)
  panel_w <- 280; panel_h <- 160
  panel_gap_x <- 24; panel_gap_y <- 18
  pad_left <- 18; pad_top <- 50; pad_bot <- 28
  width  <- pad_left + ncols * panel_w + (ncols - 1) * panel_gap_x + 12
  height <- pad_top + nrowg * panel_h + (nrowg - 1) * panel_gap_y + pad_bot

  panel_blocks <- vapply(seq_along(cohorts), function(k) {
    co <- cohorts[k]
    co_short <- sub("^TCGA-", "", co)
    rows <- d[cohort == co]
    rows[, gene := factor(gene, levels = rows$gene[order(homdel_rate)])]
    setorder(rows, -homdel_rate)
    col <- ((k - 1) %% ncols)
    rr  <- ((k - 1) %/% ncols)
    px <- pad_left + col * (panel_w + panel_gap_x)
    py <- pad_top  + rr  * (panel_h + panel_gap_y)

    n_ge <- nrow(rows)
    row_h <- (panel_h - 26) / max(1, n_ge)
    bar_x <- px + 90
    bar_w <- panel_w - 100

    # Gene rows
    grows <- vapply(seq_len(n_ge), function(i) {
      r <- rows[i]
      y <- py + 22 + (i - 0.5) * row_h
      x_hom <- bar_x + (r$homdel_rate    / 100) * bar_w
      x_bas <- bar_x + (r$baseline_rate  / 100) * bar_w
      conn <- sprintf('<line class="f5-link" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"></line>',
                       min(x_hom, x_bas), y, max(x_hom, x_bas), y)
      sprintf('<g class="f5-row" data-cohort="%s" data-gene="%s" data-homdel-rate="%.1f" data-baseline-rate="%.1f" data-n-homdel="%d" data-n-cohort="%d">
        <rect class="f5-hit" x="%d" y="%.1f" width="%d" height="%.1f"></rect>
        <text class="f5-gene" x="%d" y="%.1f">%s</text>
        %s
        <circle class="f5-baseline" cx="%.1f" cy="%.1f" r="3.5"></circle>
        <circle class="f5-homdel"   cx="%.1f" cy="%.1f" r="4"></circle>
      </g>',
        co_short, r$gene, r$homdel_rate, r$baseline_rate,
        r$n_mtap_homdel, r$n_cohort,
        px + 12, py + 22 + (i - 1) * row_h, panel_w - 16, row_h,
        bar_x - 6, y + 3.5, r$gene,
        conn,
        x_bas, y, x_hom, y)
    }, character(1))

    # X-axis ticks at 0/25/50/75/100
    xticks <- vapply(c(0, 25, 50, 75, 100), function(t) {
      x <- bar_x + (t / 100) * bar_w
      sprintf('<line class="f5-tick" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"></line><text class="f5-ticklbl" x="%.1f" y="%.1f">%d%%</text>',
              x, py + 22, x, py + panel_h - 6,
              x, py + panel_h + 8, t)
    }, character(1))

    rr0 <- rows[1]   # cohort header data
    sprintf('<g class="f5-panel">
      <rect class="f5-bg" x="%d" y="%d" width="%d" height="%d" rx="6" ry="6"></rect>
      <text class="f5-title" x="%d" y="%d">%s · CN0=%d / n=%d</text>
      %s %s
    </g>',
      px, py, panel_w, panel_h,
      px + 12, py + 14, co_short, rr0$n_mtap_homdel, rr0$n_cohort,
      paste(grows, collapse = ""), paste(xticks, collapse = ""))
  }, character(1))

  legend <- sprintf('<g class="f5-legend">
    <circle class="f5-baseline" cx="%d" cy="22" r="3.5"></circle>
    <text class="legend-text" x="%d" y="26">MTAP-intact baseline</text>
    <circle class="f5-homdel" cx="%d" cy="22" r="4"></circle>
    <text class="legend-text" x="%d" y="26">MTAP-homdel patients</text>
  </g>',
    pad_left + 4, pad_left + 14,
    pad_left + 180, pad_left + 192)

  sprintf('<svg class="figure-svg fig5-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Mutation landscape of MTAP-homdel patients">
    %s %s
    <text class="fig-axis-x" x="%d" y="%d" text-anchor="middle">Mutation rate (%%)</text>
  </svg>',
    width, height, legend, paste(panel_blocks, collapse = ""),
    width / 2, height - 8)
}
fig5_svg <- make_figure5_svg(s3b)

# ---------------------------------------------------------------------------
# Figure 6 — clinical landscape Gantt
# ---------------------------------------------------------------------------
make_figure6_svg <- function(dt) {
  parse_d <- function(x) {
    x <- as.character(x)
    ymd <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    ym  <- suppressWarnings(as.Date(paste0(x, "-15"), format = "%Y-%m-%d"))
    fifelse(is.na(ymd), ym, ymd)
  }
  d <- copy(dt)
  d[, start_d := parse_d(start_date)]
  d[, end_d   := parse_d(completion_date)]
  d[, primary_d := parse_d(primary_completion)]
  d[, last_update_d := parse_d(last_update_date)]
  TODAY <- as.Date("2026-06-16")
  d[, x_start := start_d]
  d[, x_end := fcase(
    !is.na(end_d), end_d,
    !is.na(primary_d), primary_d,
    overall_status %in% c("RECRUITING","ACTIVE_NOT_RECRUITING","ENROLLING_BY_INVITATION"), TODAY,
    overall_status %in% c("NOT_YET_RECRUITING"), pmax(start_d, TODAY, na.rm = TRUE) + 365,
    default = pmax(start_d, last_update_d, na.rm = TRUE))]
  d <- d[!is.na(x_start)]
  class_levels <- c("MTA-cooperative PRMT5i", "MAT2A inhibitor",
                    "First-gen PRMT5i (SAM-competitive)")
  d[, mechanism_class := factor(mechanism_class, levels = class_levels)]
  # Order: class → drug → start
  drug_order <- d[, .(max_phase = max(phase, na.rm = TRUE),
                      earliest = min(x_start, na.rm = TRUE)),
                   by = .(mechanism_class, canonical_drug)
                  ][order(mechanism_class, earliest)]
  d[, drug_ord := match(canonical_drug, drug_order$canonical_drug)]
  setorder(d, mechanism_class, drug_ord, x_start)
  n <- nrow(d)

  row_h <- 17L
  row_gap <- 4L
  pad_left <- 200L
  pad_top  <- 56L
  pad_bot  <- 40L
  pad_right<- 30L
  date_w   <- 700L
  width    <- pad_left + date_w + pad_right
  height   <- pad_top + n * (row_h + row_gap) + pad_bot

  d_min <- as.Date("2016-01-01"); d_max <- as.Date("2029-06-01")
  date_to_x <- function(dd) pad_left + as.numeric(dd - d_min) / as.numeric(d_max - d_min) * date_w

  # Year ticks
  years <- seq(2016, 2029)
  tick_lines <- vapply(years, function(yr) {
    dd <- as.Date(sprintf("%d-01-01", yr))
    if (dd < d_min || dd > d_max) return("")
    x <- date_to_x(dd)
    sprintf('<line class="f6-grid" x1="%.1f" y1="%d" x2="%.1f" y2="%d"></line><text class="f6-yrlbl" x="%.1f" y="%d">%d</text>',
            x, pad_top - 6, x, height - pad_bot + 4, x, height - pad_bot + 22, yr)
  }, character(1))

  # Phase colors
  phase_fill <- function(p) {
    switch(as.character(p),
      "Ph1" = "#9FE1CB", "Ph1/Ph2" = "#5DCAA5",
      "Ph2" = "#1D9E75", "Ph2/Ph3" = "#0F6E56",
      "Ph3" = "#000000", "EARLY_PHASE1" = "#9FE1CB",
      "#D3D1C7")
  }
  status_marker <- function(s) {
    switch(as.character(s),
      "RECRUITING"="●", "ACTIVE_NOT_RECRUITING"="●",
      "ENROLLING_BY_INVITATION"="●",
      "NOT_YET_RECRUITING"="◦",
      "COMPLETED"="◆", "TERMINATED"="✕",
      "WITHDRAWN"="✕", "SUSPENDED"="✕", "?")
  }

  row_blocks <- vapply(seq_len(n), function(i) {
    r <- d[i]
    y <- pad_top + (i - 1) * (row_h + row_gap)
    x1 <- date_to_x(r$x_start)
    x2 <- date_to_x(r$x_end)
    bar_w <- pmax(2, x2 - x1)
    fill  <- phase_fill(r$phase)
    label_drug <- sub(" \\(.*$", "", r$canonical_drug)
    sprintf('<g class="f6-row" data-nct="%s" data-drug="%s" data-class="%s" data-phase="%s" data-status="%s" data-start="%s" data-end="%s" data-title="%s">
      <rect class="f6-hit" x="0" y="%d" width="%d" height="%d"></rect>
      <text class="f6-drug" x="%d" y="%.1f">%s</text>
      <text class="f6-nct"  x="%d" y="%.1f">%s</text>
      <rect class="f6-bar" x="%.1f" y="%d" rx="3" ry="3" height="%d" fill="%s" width="%.2f"></rect>
      <text class="f6-marker" x="%.1f" y="%.1f">%s</text>
    </g>',
      esc(r$nct_id), esc(r$canonical_drug), esc(as.character(r$mechanism_class)),
      esc(as.character(r$phase)), esc(as.character(r$overall_status)),
      esc(as.character(r$x_start)), esc(as.character(r$x_end)),
      esc(r$brief_title),
      y - 2, width, row_h + 4,
      pad_left - 145, y + row_h / 2 + 4, esc(label_drug),
      pad_left - 70, y + row_h / 2 + 4, esc(r$nct_id),
      x1, y, row_h, fill, bar_w,
      x1 - 12, y + row_h / 2 + 4, status_marker(r$overall_status)
    )
  }, character(1))

  legend <- sprintf('<g class="f6-legend">
    <text class="legend-text" x="%d" y="20">Phase</text>
    <rect x="%d" y="14" width="16" height="9" rx="2" fill="%s"></rect><text class="legend-text" x="%d" y="22">Ph1</text>
    <rect x="%d" y="14" width="16" height="9" rx="2" fill="%s"></rect><text class="legend-text" x="%d" y="22">Ph1/2</text>
    <rect x="%d" y="14" width="16" height="9" rx="2" fill="%s"></rect><text class="legend-text" x="%d" y="22">Ph2</text>
    <rect x="%d" y="14" width="16" height="9" rx="2" fill="%s"></rect><text class="legend-text" x="%d" y="22">Ph2/3</text>
    <text class="legend-text" x="%d" y="20">Status</text>
    <text class="f6-marker" x="%d" y="22">●</text><text class="legend-text" x="%d" y="22">Active</text>
    <text class="f6-marker" x="%d" y="22">◦</text><text class="legend-text" x="%d" y="22">Planned</text>
    <text class="f6-marker" x="%d" y="22">◆</text><text class="legend-text" x="%d" y="22">Completed</text>
    <text class="f6-marker" x="%d" y="22">✕</text><text class="legend-text" x="%d" y="22">Terminated</text>
  </g>',
    pad_left,
    pad_left + 50, "#9FE1CB", pad_left + 70,
    pad_left + 110, "#5DCAA5", pad_left + 130,
    pad_left + 180, "#1D9E75", pad_left + 200,
    pad_left + 250, "#0F6E56", pad_left + 270,
    pad_left + 320,
    pad_left + 370, pad_left + 380,
    pad_left + 420, pad_left + 432,
    pad_left + 480, pad_left + 492,
    pad_left + 552, pad_left + 562
  )

  sprintf('<svg class="figure-svg fig6-svg" viewBox="0 0 %d %d" preserveAspectRatio="xMinYMin meet" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Clinical landscape Gantt timeline">
    %s
    %s
    %s
    <text class="fig-axis-x" x="%d" y="%d" text-anchor="middle">Year</text>
  </svg>',
    width, height,
    legend,
    paste(tick_lines, collapse = ""),
    paste(row_blocks, collapse = ""),
    pad_left + date_w / 2, height - 6
  )
}
fig6_svg <- make_figure6_svg(trials)

s1_top <- s1[order(-homdel_pct)][1:8]
s1_top_pct <- vapply(seq_len(nrow(s1_top)), function(i) {
  sprintf('<li><span class="cohort">%s</span> <b>%.1f%%</b> homdel · %.1f%% hetdel (n=%d)</li>',
          sub("^TCGA-", "", s1_top[i]$cohort),
          s1_top[i]$homdel_pct, s1_top[i]$hetdel_pct, s1_top[i]$n_cohort)
}, character(1))

# Confirm rank concordance between TCGA and MSK-IMPACT (for the annotation)
.tcga_rank <- s1[order(-homdel_pct), .(cohort, r_tcga = .I)]
.msk_rank  <- s5[order(-msk_mtap_pct), .(cohort = tcga, r_msk = .I)]
.joined    <- merge(.tcga_rank, .msk_rank, by = "cohort")
rank_rho <- if (nrow(.joined) >= 5) suppressWarnings(round(cor(.joined$r_tcga, .joined$r_msk, method = "spearman"), 2)) else NA_real_
n_msk_ind  <- nrow(s5)

prev_intro <- glue('
  <p class="msk-note">
    <b>Real-world cross-check.</b> The indication rank order in Figure 1
    replicates in MSK-IMPACT 50K (real-world panel-sequenced cohort,
    n ≈ 54k; ρ<sub>rank</sub> = {rank_rho} across {n_msk_ind} matched
    indications).  Absolute homdel rates run ~5–18× lower in MSK-IMPACT
    because panel-based GISTIC −2 under-detects focal homdel relative to
    whole-exome ABSOLUTE — this reflects a methodological difference in
    detection, and is independently supported by matched-sample IHC vs
    panel-NGS comparisons in NSCLC and mesothelioma.
    {cite_html(c("taylor2018", "cerami2012", "brune2026_mtap_ihc", "febres_aldana_2024_mtap_meso"))}
  </p>')

section1 <- glue('
<section id="genomic-landscape" class="section">
  <div class="sec-h">
    <div class="sec-num">1</div>
    <div>
      <div class="sec-title">Genomic landscape</div>
      <div class="sec-q">Where is MTAP loss prevalent and what comes along with it?</div>
    </div>
  </div>

  <div class="fig-pair">
    <div class="fig-pair-col">
      <h3>Figure 1 · Deletion prevalence across TCGA cohorts</h3>
      <p>
        Figure 1 indicates that MTAP homozygous deletion (dark teal bars)
        is most prevalent in GBM (~43 %), MESO (~36 %), BLCA (~23 %) and
        PAAD (~21 %), followed by ESCA, LUSC, SKCM and HNSC (~11–18 %).
        Heterozygous loss (CN = 1, light teal bars) is more broadly
        distributed across cohorts and reaches comparable or higher
        per-cohort rates in some indications (e.g.&nbsp;SKCM ~38 %,
        CHOL ~36 %, LUSC ~31 %).  {cite_html(c("taylor2018"))}
      </p>
      <figure class="fig-wrap" data-fig="figure1">
        {s1_svg}
        <figcaption class="fig-cap">
          <b>How this was built.</b> For each of 33 TCGA cancer cohorts we
          counted the percentage of primary-tumour samples carrying
          homozygous (copy-number = 0, dark teal) or heterozygous (CN = 1,
          light teal) deletion at the <i>MTAP</i> locus.  Copy-number calls
          come from the ABSOLUTE algorithm (PanCanAtlas / Taylor 2018,
          hg19) — a tumour-purity- and ploidy-corrected method that
          produces integer allelic CN per segment.  For each sample, MTAP
          CN is the overlap-length-weighted CN across all ABSOLUTE
          segments that touch the gene body.  Filters: primary tumour
          (sample-type 01 or 03), ABSOLUTE QC pass, n ≥ 20 per cohort.
          Cohorts are ranked by homdel %; hover any row for the exact
          numbers.
        </figcaption>
      </figure>
      {prev_intro}
    </div>
    <div class="fig-pair-col">
      <h3>Figure 2 · Copy-number to expression fidelity</h3>
      <p>
        Figure 2 indicates that the deletion is generally functionally
        penetrant.  In cohorts with enough CN = 0 samples to estimate it,
        MTAP median expression falls monotonically from CN = 2 → CN = 0.
        The per-cohort Spearman ρ between CN and expression correlates
        with cohort homdel % at ≈ 0.83, so the cohorts with the most
        homdel also tend to show the tightest CN → expression coupling.
        {cite_html(c("kryukov2016", "wilks2021"))}
      </p>
      <figure class="fig-wrap" data-fig="figure2">
        {fig2_svg}
        <figcaption class="fig-cap">
          <b>How this was built.</b> For every patient we joined the MTAP
          DNA-level copy-number call (from ABSOLUTE, same as Figure 1)
          with the matched MTAP mRNA expression from recount3 (Wilks
          2021 — a uniformly re-processed pan-TCGA RNA-seq resource).  We
          binned samples by allelic CN (0, 1, 2, 3+) and plotted the
          median log<sub>2</sub>(TPM + 1) per bin as a connected trajectory
          per cohort; dot size scales with sample count per bin.  Per
          cohort we also computed the Spearman rank correlation (ρ)
          between CN and expression, shown only when at least 3 CN = 0
          samples are available.  Panels are ordered by ρ (largest
          first).  Hover any panel for per-bin medians and counts.
        </figcaption>
      </figure>
    </div>
  </div>

  <div class="fig-pair">
    <div class="fig-pair-col">
      <h3>Figure 3 · Focality of the deletion</h3>
      <p>
        Figure 3 indicates that homozygous and heterozygous deletions at
        the MTAP locus have different size distributions.  Homozygous
        deletion is predominantly <b>focal</b> (median ≈ 1.5–2.8 Mb,
        &lt; 3 % arm-level), consistent with a driver-selected event.
        Heterozygous loss is mostly <b>arm-level</b> (median ≈ 25–115 Mb,
        56–86 % arm-level), consistent with a passenger 9p loss.  The
        MTA accumulation that creates the apparent PRMT5 dependency is
        generally expected to require complete focal-homozygous MTAP loss.
        {cite_html("kryukov2016")}
      </p>
      <figure class="fig-wrap" data-fig="figure3">
        {fig3_svg}
        <figcaption class="fig-cap">
          <b>How this was built.</b> For every patient with an MTAP deletion
          (CN ≤ 1) we identified the contiguous ABSOLUTE chr-9p segment
          carrying the same CN call that spans the MTAP locus, merged
          adjacent same-CN segments separated by ≤ 500 kb, and measured the
          resulting deletion footprint in megabases.  Each footprint is
          classified as <b>focal</b> (&lt; 3 Mb — typically the narrow
          9p21.3 driver event), <b>intermediate</b> (3–25 Mb), or
          <b>arm-level</b> (≥ 25 Mb, or ≥ 50% of 9p — usually a passenger).
          Per cohort, we then summarise the % of deleted patients in each
          class, separately for heterozygous (CN = 1) and homozygous
          (CN = 0) deletions.  Cohorts ranked as in Figure 1; hover any row
          for the exact percentages and the median footprint size.
        </figcaption>
      </figure>
    </div>
    <div class="fig-pair-col">
      <h3>Figure 4 · Co-deletion partners</h3>
      <p>
        Figure 4 indicates that CDKN2A and CDKN2B are co-deleted in
        ≈ 88–100 % of MTAP-homdel patients in every cohort with at least
        5 deleted samples.  The IFN cluster (IFNA1 / B1 / W1) and
        ELAVL2 are reliably pulled into the deletion footprint as well.
        Distant tumour suppressors on other chromosomes (PTEN, RB1,
        TP53, NF1) appear largely untouched in MTAP-homdel patients,
        consistent with MTAP loss representing a focal 9p21 event.
        {cite_html(c("zhang2018", "taylor2018"))}
      </p>
      <figure class="fig-wrap" data-fig="figure4">
        {fig4_svg}
        <figcaption class="fig-cap">
          <b>How this was built.</b> For each MTAP-homdel patient we
          measured the copy-number state at every neighbouring gene of
          interest, using the same overlap-weighted ABSOLUTE CN as Figures
          1–3.  "Anyloss" means CN ≤ 1 (homozygous + heterozygous loss
          combined).  The <b>cell colour</b> is the enrichment in anyloss%
          among MTAP-homdel patients vs the MTAP-intact baseline of the
          same cohort, expressed in Δ percentage points (red = under-
          represented, blue = enriched).  The <b>cell label</b> is the raw
          anyloss% in the MTAP-homdel subset.  Genes are grouped by
          chromosomal proximity to MTAP: focal 9p21 neighbours (top), 9p
          arm-level genes (middle, separated by dashed line), and distant
          tumour suppressors on other chromosomes (bottom).  Only cohorts
          with ≥ 5 MTAP-homdel patients are shown.  Hover any cell for n,
          homdel%, and enrichment value.
        </figcaption>
      </figure>
    </div>
  </div>

  <p class="aside">Per-cohort mutational profiles of MTAP-homdel patients (e.g.&nbsp;PAAD ≈ KRAS-mut + TP53 + SMAD4; GBM ≈ TP53 + EGFR + PTEN; BLCA ≈ FGFR3 + TP53 + KMT2D; LUSC ≈ KMT2D + PIK3CA + NFE2L2; HNSC ≈ TP53 + CASP8 + NOTCH1; MESO ≈ low-TMB BAP1 / NF2-driven) are kept in the audit trail (<code>03b_mutpop_long.parquet</code>) rather than shown as a separate figure here — the indication-level signatures are reused inline in the per-indication combination strategy in Section 4. {cite_html(c("ellrott2018", "chakravarty2017"))}</p>
</section>')

# ============================================================================
# Section 2 — Mechanism & rationale (from panel2_mechanism.yaml)
# ============================================================================
flow_blocks <- vapply(p2$flow, function(s) {
  sprintf('<div class="step">
    <div class="step-num">%d</div>
    <div class="step-body">
      <div class="step-label">%s</div>
      <div class="step-text">%s %s</div>
    </div>
  </div>', s$id, esc(s$label), esc(s$text), cite_html(s$citations))
}, character(1))

bm_blocks <- vapply(p2$biomarker, function(b) {
  sprintf('<li><div class="bm-point">%s</div><div class="bm-detail">%s %s</div></li>',
          esc(b$point), esc(b$detail), cite_html(b$citations))
}, character(1))

strat_blocks <- vapply(p2$strategies, function(s) {
  sprintf('<div class="strat"><div class="strat-class">%s</div><div class="strat-text">%s %s</div></div>',
          esc(s$class), esc(s$rationale), cite_html(s$citations))
}, character(1))

uncert_blocks <- vapply(p2$uncertainties, function(u) {
  if (is.list(u)) sprintf('<li>%s %s</li>', esc(u$text), cite_html(u$citations))
  else            sprintf('<li>%s</li>', esc(u))
}, character(1))

section2 <- glue('
<section id="mechanism" class="section">
  <div class="sec-h">
    <div class="sec-num">2</div>
    <div>
      <div class="sec-title">Mechanism &amp; rationale</div>
      <div class="sec-q">{esc(p2$question)}</div>
    </div>
  </div>
  <div class="lead">{esc(p2$summary)}</div>

  <h3>2.1 · Mechanism flow</h3>
  <div class="flow">{paste(flow_blocks, collapse = "")}</div>

  <h3>2.2 · Biomarker rationale</h3>
  <ul class="bm">{paste(bm_blocks, collapse = "")}</ul>

  <h3>2.3 · Therapeutic strategy classes</h3>
  {paste(strat_blocks, collapse = "")}

  <h3>2.4 · Acknowledged uncertainties</h3>
  <ul class="uncert">{paste(uncert_blocks, collapse = "")}</ul>
</section>')

# ============================================================================
# Section 3 — Clinical landscape (collapsible drug/trial cards)
# ============================================================================
status_class <- function(s) {
  if (is.na(s)) return("st-unknown")
  paste0("st-", tolower(as.character(s)))
}
phase_class <- function(p) {
  if (is.na(p)) return("ph-unknown")
  paste0("ph-", gsub("/","", tolower(as.character(p))))
}

bm_pill <- function(s) {
  if (grepl("MTAP \\+ CDKN2A", s))           return('<span class="pill bm-strict">MTAP + CDKN2A homdel</span>')
  if (grepl("MTAP homdel required", s))      return('<span class="pill bm-strict">MTAP homdel required</span>')
  if (grepl("CDKN2A homdel required", s))    return('<span class="pill bm-strict">CDKN2A homdel required</span>')
  if (grepl("status reported", s))           return('<span class="pill bm-soft">MTAP reported, not required</span>')
  if (grepl("9p21", s))                      return('<span class="pill bm-soft">9p21 loss referenced</span>')
  '<span class="pill bm-none">Not biomarker-selected</span>'
}
combo_pill <- function(s) {
  if (is.na(s)) return("")
  if (s == "Monotherapy")            return('<span class="pill cb-mono">Monotherapy</span>')
  if (grepl("KRAS", s))              return(sprintf('<span class="pill cb-kras">%s</span>', esc(s)))
  if (grepl("EGFR", s))              return(sprintf('<span class="pill cb-egfr">%s</span>', esc(s)))
  if (grepl("IO", s))                return(sprintf('<span class="pill cb-io">%s</span>',   esc(s)))
  if (grepl("PARP", s))              return(sprintf('<span class="pill cb-parp">%s</span>', esc(s)))
  if (grepl("chemo", s))             return(sprintf('<span class="pill cb-chemo">%s</span>',esc(s)))
  sprintf('<span class="pill cb-other">%s</span>', esc(s))
}

trial_card <- function(r) {
  nct <- r$nct_id
  readout <- p3r[[nct]]
  readout_html <- if (is.null(readout)) "" else {
    finds <- paste(sprintf('<li>%s</li>',
                            vapply(readout$key_findings, esc, character(1))),
                    collapse = "")
    extras <- if (nz(readout$additional_citations))
      sprintf(' · also %s',
              paste(vapply(readout$additional_citations, cite_chip, character(1)),
                    collapse = " "))
    else ""
    sprintf('<div class="readout"><div class="readout-head">📊 %s · %s%s</div><ul>%s</ul></div>',
            esc(readout$headline), cite_chip(readout$citation), extras, finds)
  }

  elig_excerpt <- esc(substr(r$eligibility_excerpt, 1, 700))
  elig_html <- if (nz(elig_excerpt))
    sprintf('<details class="elig"><summary>Eligibility excerpt</summary><div class="elig-body">%s%s</div></details>',
            elig_excerpt,
            ifelse(nchar(r$eligibility_excerpt) > 700, " <em>…(truncated)</em>", ""))
    else ""

  summary_html <- if (nz(r$brief_summary))
    sprintf('<div class="brief"><b>Summary:</b> %s</div>', esc(r$brief_summary)) else ""
  endpoint_html <- if (nz(r$primary_endpoints))
    sprintf('<div class="endpoint"><b>Primary endpoint(s):</b> %s</div>',
            esc(r$primary_endpoints)) else ""
  interv_html <- if (nz(r$interventions_all))
    sprintf('<div class="interv"><b>Interventions:</b> %s</div>',
            esc(r$interventions_all)) else ""

  sprintf('
  <details class="trial">
    <summary>
      <span class="trial-head">
        <a class="nct" href="https://clinicaltrials.gov/study/%s" target="_blank" rel="noopener">%s</a>
        <span class="pill %s">%s</span>
        <span class="pill %s">%s</span>
        %s %s
      </span>
      <span class="trial-title">%s</span>
      <span class="trial-meta">indications: %s · start %s · updated %s</span>
    </summary>
    <div class="trial-body">
      %s %s %s %s %s
    </div>
  </details>',
    esc(nct), esc(nct),
    phase_class(r$phase_short), esc(r$phase_short),
    status_class(r$status_short), esc(r$status_short),
    bm_pill(r$biomarker_strategy),
    combo_pill(r$combination_strategy),
    esc(r$brief_title),
    esc(r$indications_short %||% ""),
    esc(r$start_date), esc(r$last_update_date),
    readout_html, summary_html, endpoint_html, interv_html, elig_html)
}

drug_card <- function(d) {
  rows <- trials[canonical_drug == d$name]
  if (nrow(rows) == 0) {
    return(sprintf('<details class="drug"><summary><b>%s</b> <span class="empty">no CT.gov entries</span></summary></details>',
                   esc(d$name)))
  }
  rows[, .phase_n := as.integer(phase_short)]
  setorder(rows, -.phase_n, status_short, start_date)
  rows[, .phase_n := NULL]
  rr <- rollup[canonical_drug == d$name]

  summary_line <- sprintf(
    'max phase <b>%s</b> · <span class="st-active">%d active</span> · <span class="st-planned">%d planned</span> · <span class="st-completed">%d completed</span> · <span class="st-terminated">%d terminated</span>',
    esc(rr$max_phase), rr$n_active, rr$n_planned, rr$n_completed, rr$n_terminated)

  pub_chip <- if (nz(d$primary_publication))
    sprintf(' · primary publication %s', cite_chip(d$primary_publication)) else ""

  aliases <- if (length(d$aliases))
    sprintf(' <span class="alias">aka %s</span>', esc(paste(d$aliases, collapse = ", "))) else ""

  notes_html <- if (!is.null(d$notes) && nz(d$notes))
    sprintf('<div class="notes">%s</div>', esc(trimws(d$notes))) else ""

  trial_blocks <- vapply(seq_len(nrow(rows)), function(i) trial_card(rows[i]), character(1))

  sprintf('
  <details class="drug">
    <summary>
      <span class="drug-name">%s</span>%s
      <span class="mech">%s · %s</span>
      <span class="rollup">%s%s</span>
    </summary>
    <div class="drug-body">%s %s</div>
  </details>',
    esc(d$name), aliases, esc(d$mechanism_class), esc(d$sponsor),
    summary_line, pub_chip, notes_html,
    paste(trial_blocks, collapse = ""))
}

# ---------------------------------------------------------------------------
# Filterable trial table (replaces the Gantt)
# ---------------------------------------------------------------------------
make_trial_table <- function() {
  # Build short biomarker / combo tags consistent with the pills used elsewhere
  bm_tag <- function(s) {
    if (grepl("MTAP \\+ CDKN2A", s))           return("MTAP+CDKN2A")
    if (grepl("MTAP homdel required", s))      return("MTAP homdel")
    if (grepl("CDKN2A homdel required", s))    return("CDKN2A homdel")
    if (grepl("status reported", s))           return("MTAP reported")
    if (grepl("9p21", s))                      return("9p21 referenced")
    "Not selected"
  }

  rows <- trials[, .phase_n := as.integer(phase_short)]
  setorder(rows, mechanism_class, canonical_drug, -.phase_n, start_date)
  rows[, .phase_n := NULL]

  # Short mechanism label
  mech_short <- function(m) {
    if (grepl("MTA-cooperative", m))               return("MTA-coop PRMT5i")
    if (grepl("MAT2A", m))                          return("MAT2A inh")
    if (grepl("SAM-competitive", m))                return("1st-gen PRMT5i")
    sub(" \\(.+$", "", m)
  }

  tr_rows <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i]
    readout <- p3r[[r$nct_id]]
    bm   <- bm_tag(r$biomarker_strategy)
    combo<- as.character(r$combination_strategy)
    phase<- as.character(r$phase_short)
    status<-as.character(r$status_short)
    has_readout <- !is.null(readout)
    ct_results <- isTRUE(r$has_results)
    mech_lbl <- mech_short(r$mechanism_class)
    mech_pill_cls <- if (grepl("MTA-coop", mech_lbl)) "mech-mta"   else
                    if (grepl("MAT2A",    mech_lbl)) "mech-mat2a" else "mech-fg"

    # ORR / N from curated readouts
    orr_pct  <- if (has_readout && !is.null(readout$orr_pct)  && !is.na(readout$orr_pct))  readout$orr_pct  else NA_real_
    n_eval   <- if (has_readout && !is.null(readout$n_evaluable) && !is.na(readout$n_evaluable)) readout$n_evaluable else NA_integer_
    orr_ctx  <- if (has_readout && !is.null(readout$orr_context) && nzchar(readout$orr_context)) readout$orr_context else NA_character_
    orr_disp <- if (is.na(orr_pct)) "—" else sprintf("%.1f%%", orr_pct)
    # N cell: "n_evaluable · indication" so the ORR number is anchored to a population
    n_disp   <- if (is.na(n_eval) && is.na(orr_ctx))           "—"
                else if (is.na(n_eval))                         sprintf('<span class="n-ctx">%s</span>',                       esc(orr_ctx))
                else if (is.na(orr_ctx))                        as.character(n_eval)
                else                                            sprintf('%d <span class="n-ctx">· %s</span>', n_eval, esc(orr_ctx))
    orr_title <- if (!is.null(readout$orr_note)) esc(readout$orr_note) else ""

    results_cell <- if (has_readout) {
      sprintf('<button class="results-toggle" type="button" aria-expanded="false">📄 readout</button>')
    } else if (ct_results) {
      sprintf('<a class="results-link" href="https://clinicaltrials.gov/study/%s?tab=results" target="_blank" rel="noopener">CT.gov</a>', esc(r$nct_id))
    } else {
      '<span class="results-none">—</span>'
    }

    # Build the expandable detail row content
    detail_blocks <- c()
    if (has_readout) {
      finds <- paste(sprintf('<li>%s</li>', vapply(readout$key_findings, esc, character(1))),
                      collapse = "")
      extras <- if (nz(readout$additional_citations))
        sprintf(' · also %s',
                paste(vapply(readout$additional_citations, cite_chip, character(1)),
                      collapse = " "))
      else ""
      detail_blocks <- c(detail_blocks,
        sprintf('<div class="readout"><div class="readout-head">📊 %s · %s%s</div><ul>%s</ul></div>',
                esc(readout$headline), cite_chip(readout$citation), extras, finds))
    }
    if (nz(r$brief_summary))
      detail_blocks <- c(detail_blocks,
        sprintf('<div class="trial-detail-row"><b>Summary:</b> %s</div>', esc(r$brief_summary)))
    if (nz(r$primary_endpoints))
      detail_blocks <- c(detail_blocks,
        sprintf('<div class="trial-detail-row"><b>Primary endpoint(s):</b> %s</div>', esc(r$primary_endpoints)))
    if (nz(r$interventions_all))
      detail_blocks <- c(detail_blocks,
        sprintf('<div class="trial-detail-row"><b>Interventions:</b> %s</div>', esc(r$interventions_all)))
    elig_excerpt <- esc(substr(r$eligibility_excerpt, 1, 1200))
    if (nz(elig_excerpt))
      detail_blocks <- c(detail_blocks,
        sprintf('<div class="trial-detail-row"><b>Eligibility excerpt:</b><div class="elig-body">%s%s</div></div>',
                elig_excerpt,
                ifelse(nchar(r$eligibility_excerpt) > 1200, " <em>…(truncated)</em>", "")))
    detail_html <- paste(detail_blocks, collapse = "")

    # Filterable + sortable attributes — used by JS
    phase_num <- match(phase, c("Ph1","Ph1/2","Ph2","Ph2/3","Ph3"))
    if (is.na(phase_num)) phase_num <- 0L
    status_num <- match(status, c("Active","Planned","Completed","Terminated"))
    if (is.na(status_num)) status_num <- 5L
    data_attrs <- sprintf(
      'data-search="%s" data-class="%s" data-phase="%s" data-status="%s" data-biomarker="%s" data-combo="%s" data-has-results="%s" data-sort-drug="%s" data-sort-mech="%s" data-sort-phase="%d" data-sort-status="%d" data-sort-orr="%s" data-sort-n="%s" data-sort-start="%s"',
      esc(tolower(paste(r$nct_id, r$canonical_drug, r$brief_title,
                        r$conditions, r$keywords, r$canonical_sponsor, sep = " "))),
      esc(r$mechanism_class), esc(phase), esc(status), esc(bm), esc(combo),
      if (has_readout || ct_results) "yes" else "no",
      esc(tolower(r$canonical_drug)),
      esc(tolower(mech_lbl)),
      phase_num, status_num,
      if (is.na(orr_pct)) "-1" else sprintf("%.3f", orr_pct),
      if (is.na(n_eval))  "-1" else as.character(n_eval),
      esc(r$start_date %||% "")
    )

    sprintf('
    <tr class="tt-row" %s>
      <td class="tt-nct"><a href="https://clinicaltrials.gov/study/%s" target="_blank" rel="noopener">%s</a></td>
      <td class="tt-drug"><b>%s</b><div class="tt-sponsor">%s</div></td>
      <td><span class="pill %s">%s</span></td>
      <td><span class="pill ph-%s">%s</span></td>
      <td><span class="pill st-%s">%s</span></td>
      <td class="tt-indication">%s</td>
      <td><span class="pill bm-%s">%s</span></td>
      <td><span class="pill cb-%s">%s</span></td>
      <td class="tt-orr" title="%s">%s</td>
      <td class="tt-n">%s</td>
      <td class="tt-date">%s</td>
      <td class="tt-results">%s</td>
    </tr>
    <tr class="tt-detail-row" hidden>
      <td colspan="12">
        <div class="tt-detail-body">%s</div>
      </td>
    </tr>',
      data_attrs,
      esc(r$nct_id), esc(r$nct_id),
      esc(r$canonical_drug), esc(r$canonical_sponsor),
      mech_pill_cls, esc(mech_lbl),
      gsub("/","", tolower(phase)), esc(phase),
      tolower(status), esc(status),
      esc(r$indications_short %||% ""),
      if (bm == "Not selected") "none" else
        if (grepl("MTAP", bm) || grepl("CDKN2A", bm)) "strict" else "soft",
      esc(bm),
      if (combo == "Monotherapy") "mono" else
      if (grepl("KRAS", combo)) "kras" else
      if (grepl("EGFR", combo)) "egfr" else
      if (grepl("IO", combo))   "io"   else
      if (grepl("PARP", combo)) "parp" else
      if (grepl("chemo", combo))"chemo" else "other",
      esc(combo),
      orr_title, orr_disp,
      n_disp,
      esc(r$start_date %||% ""),
      results_cell,
      detail_html
    )
  }, character(1))

  # Dropdown options
  unique_or_empty <- function(x) {
    v <- unique(stats::na.omit(as.character(x)))
    v[nzchar(v)]
  }
  class_opts  <- unique_or_empty(rows$mechanism_class)
  phase_opts  <- c("Ph1","Ph1/2","Ph2","Ph2/3","Ph3")
  status_opts <- c("Active","Planned","Completed","Terminated")
  bm_opts     <- c("MTAP homdel","MTAP+CDKN2A","CDKN2A homdel","MTAP reported","9p21 referenced","Not selected")

  opt_html <- function(opts) paste(sprintf('<option value="%s">%s</option>', esc(opts), esc(opts)),
                                    collapse = "")

  sprintf('
<div class="trial-table-wrap">
  <div class="filters">
    <label class="filter-search">
      <span>🔎</span>
      <input type="search" id="tt-search" placeholder="Search NCT, drug, title, indication, sponsor…" autocomplete="off">
    </label>
    <select id="tt-class"  class="filter-select"><option value="">All classes</option>%s</select>
    <select id="tt-phase"  class="filter-select"><option value="">All phases</option>%s</select>
    <select id="tt-status" class="filter-select"><option value="">All statuses</option>%s</select>
    <select id="tt-bm"     class="filter-select"><option value="">All biomarker tags</option>%s</select>
    <label class="filter-toggle">
      <input type="checkbox" id="tt-has-results">
      <span>Has readout</span>
    </label>
    <button id="tt-clear" type="button" class="filter-clear">Clear</button>
    <span class="tt-count"><span id="tt-visible">%d</span> / %d trials</span>
  </div>
  <div class="tt-scroll">
    <table class="trial-table">
      <thead>
        <tr>
          <th class="tt-sort" data-key="nct"      data-type="text">NCT ID <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="drug"     data-type="text">Drug · sponsor <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="mech"     data-type="text">Drug class <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="phase"    data-type="num">Phase <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="status"   data-type="num">Status <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="ind"      data-type="text">Indications (trial) <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="bm"       data-type="text">Enrolment biomarker <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="combo"    data-type="text">Combination strategy <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="orr"      data-type="num">Best ORR <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="n"        data-type="num">N · readout context <span class="sort-ind"></span></th>
          <th class="tt-sort" data-key="start"    data-type="text">Start date <span class="sort-ind"></span></th>
          <th>Readout</th>
        </tr>
      </thead>
      <tbody>%s</tbody>
    </table>
  </div>
</div>',
    opt_html(class_opts), opt_html(phase_opts), opt_html(status_opts), opt_html(bm_opts),
    nrow(rows), nrow(rows),
    paste(tr_rows, collapse = "")
  )
}
trial_table_html <- make_trial_table()

classes <- unique(vapply(p3d, function(d) d$mechanism_class, character(1)))
class_blocks <- vapply(classes, function(cl) {
  idx <- which(vapply(p3d, function(d) d$mechanism_class == cl, logical(1)))
  sprintf('<div class="class"><h4>%s</h4>%s</div>',
          esc(cl),
          paste(vapply(idx, function(i) drug_card(p3d[[i]]), character(1)),
                collapse = ""))
}, character(1))

clin_lead <- glue('{nrow(trials)} ClinicalTrials.gov entries across {uniqueN(trials$canonical_drug)} drugs in {length(classes)} mechanism classes. Click drugs to expand; click trials for biomarker / combination / endpoint detail and (where public) curated readouts.')

section3 <- glue('
<section id="clinical" class="section">
  <div class="sec-h">
    <div class="sec-num">3</div>
    <div>
      <div class="sec-title">Clinical landscape</div>
      <div class="sec-q">What\'s currently being tested in patients, in whom, and with what strategy?</div>
    </div>
  </div>
  <div class="lead">{clin_lead}</div>

  <h3>Trial table (filterable + sortable)</h3>
  <p class="lead">
    Every ClinicalTrials.gov entry for an MTAP-axis drug. Filter by text,
    mechanism, phase, status, or biomarker — the table updates live. Click
    any column header to sort (click again to reverse). Click any row to
    expand the trial card with eligibility excerpt, primary endpoint, and
    curated readouts where public (📄 button).
  </p>
  <p class="aside">
    <b>How this table was built.</b> We queried the ClinicalTrials.gov v2
    REST API for every search term associated with each MTAP-axis drug
    (mechanism class, aliases, sponsor names), then deduplicated by NCT
    ID.  The <i>biomarker</i> and <i>combination</i> classifiers per row
    are derived from the trial eligibility text and intervention list
    (e.g.&nbsp;explicit mention of "MTAP homozygous deletion" → "MTAP
    homdel required").  ORR and N are populated from the curated readouts
    file (peer-reviewed papers and major-conference abstracts where
    available; non-peer-reviewed press disclosures are tagged in the
    readout headline).  Where ClinicalTrials.gov hosts a results section
    but no curated readout exists, the cell links to the CT.gov results
    page.
  </p>
  {trial_table_html}
</section>')

# ============================================================================
# Section 4 — Synthesis (panel4_synthesis.yaml)
# ============================================================================
panels_html <- function(panels) {
  if (is.null(panels) || length(panels) == 0) return("")
  paste(vapply(panels, function(p) {
    # Figure N or Stage N → land in Section 1; Figure 6 → Section 3
    target <- if (grepl("6", p)) "#clinical" else "#genomic-landscape"
    sprintf('<a class="panel-ref" href="%s">%s</a>', target, esc(p))
  }, character(1)), collapse = " ")
}

ps_blocks <- vapply(p4$patient_selection, function(p) {
  refs <- if (is.list(p$refs)) paste(panels_html(p$refs$panels),
                                     cite_html(p$refs$citations)) else ""
  sprintf('<li><div class="ps-point">%s</div><div class="ps-detail">%s</div><div class="ps-refs">%s</div></li>',
          esc(p$point), esc(p$detail), refs)
}, character(1))

combo_blocks <- vapply(p4$combinations, function(co) {
  refs <- if (is.list(co$refs)) paste(panels_html(co$refs$panels),
                                       cite_html(co$refs$citations)) else ""
  strats <- paste(sprintf('<li>%s</li>',
                           vapply(co$leading_strategies, esc, character(1))),
                   collapse = "")
  sprintf('<div class="indication">
    <div class="ind-head"><span class="ind-name">%s</span><span class="ind-prev">MTAP homdel: %s</span></div>
    <ul class="strats">%s</ul>
    <div class="ind-rat">%s</div>
    <div class="ind-refs">%s</div>
  </div>', esc(co$indication), esc(co$mtap_prev_tcga), strats, esc(co$rationale), refs)
}, character(1))

prog_blocks <- vapply(p4$competitive$programs, function(p) {
  sprintf('<div class="prog">
    <div class="prog-head"><b>%s</b> — <span class="prog-drug">%s</span></div>
    <div class="prog-pos">%s</div>
    <div class="prog-cat"><b>Catalysts:</b> %s</div>
  </div>', esc(p$sponsor), esc(p$drug), esc(p$position), esc(p$catalysts))
}, character(1))

risk_blocks <- vapply(p4$risks, function(r) {
  refs <- paste(
    if (is.list(r$refs)) panels_html(r$refs$panels) else "",
    cite_html(r$citations %||% (if (is.list(r$refs)) r$refs$citations else NULL))
  )
  sprintf('<li><div class="risk-point">%s</div><div class="risk-detail">%s</div><div class="risk-refs">%s</div></li>',
          esc(r$point), esc(r$detail), refs)
}, character(1))

step_blocks <- vapply(p4$next_steps, function(s) {
  sprintf('<li><div class="step-act">%s</div><div class="step-why">%s %s</div></li>',
          esc(s$action), esc(s$why), cite_html(s$citations %||% NULL))
}, character(1))

section4 <- glue('
<section id="synthesis" class="section">
  <div class="sec-h">
    <div class="sec-num">4</div>
    <div>
      <div class="sec-title">{esc(p4$title)}</div>
      <div class="sec-q">{esc(p4$question)}</div>
    </div>
  </div>

  <h3>4.1 · Thesis</h3>
  <div class="thesis">{esc(p4$thesis$text)}<div class="refs">{cite_html(p4$thesis$citations)}</div></div>

  <h3>4.2 · Patient-selection strategy</h3>
  <ul class="ps">{paste(ps_blocks, collapse = "")}</ul>

  <h3>4.3 · Combination-therapy logic — by indication</h3>
  {paste(combo_blocks, collapse = "")}

  <h3>4.4 · Competitive landscape &amp; catalysts</h3>
  {paste(prog_blocks, collapse = "")}

  <h3>4.5 · Key risks &amp; open questions</h3>
  <ul class="risk">{paste(risk_blocks, collapse = "")}</ul>

  <h3>4.6 · Worth exploring next</h3>
  <ul class="steps">{paste(step_blocks, collapse = "")}</ul>
</section>')

# ============================================================================
# Section R — References & audit log
# ============================================================================
ref_blocks <- vapply(REF_KEYS, function(k) {
  c <- cit[[k]]
  if (is.null(c)) return(sprintf('<li id="ref-%s">[%s? — missing in citations.yaml]</li>',
                                  esc(k), esc(k)))
  sprintf('<li id="ref-%s"><span class="ref-authors">%s</span> <span class="ref-title">%s.</span> <em>%s</em> (%d) %s; %s. <a href="%s" target="_blank" rel="noopener" class="ref-link">link</a></li>',
          esc(k), esc(c$authors), esc(c$title), esc(c$journal), c$year,
          esc(c$volume %||% ""), esc(c$pages %||% ""), c$url)
}, character(1))

audit_rows <- vapply(seq_len(nrow(audit)), function(i) {
  r <- audit[i]
  sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
          esc(r$analysis), esc(r$source),
          esc(r$test %||% ""), esc(r$notes %||% ""))
}, character(1))

section_refs <- glue('
<section id="references" class="section">
  <div class="sec-h">
    <div class="sec-num ref-num">R</div>
    <div>
      <div class="sec-title">References &amp; audit log</div>
      <div class="sec-q">Every claim in this dashboard traces to a primary source. Every analysis carries an audit row.</div>
    </div>
  </div>

  <h3>R.1 · References (alphabetical by key)</h3>
  <ol class="refs">{paste(ref_blocks, collapse = "")}</ol>

  <h3>R.2 · Audit log ({nrow(audit)} rows)</h3>
  <div class="audit-wrap">
    <table class="audit">
      <thead><tr><th>analysis</th><th>source</th><th>test</th><th>notes</th></tr></thead>
      <tbody>{paste(audit_rows, collapse = "")}</tbody>
    </table>
  </div>
</section>')

# ============================================================================
# Final HTML
# ============================================================================
gen_date <- format(Sys.time(), "%Y-%m-%d %H:%M")
title <- glue("{TARGET_GENE} target intelligence dashboard")

html <- glue('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0, viewport-fit=cover">
<meta name="theme-color" content="#0F6E56">
<meta name="description" content="MTAP / PRMT5 target-intelligence dashboard — genomic landscape, mechanism, clinical landscape, and synthesis. Built from TCGA ABSOLUTE + recount3 + MC3 + ClinicalTrials.gov.">
<meta property="og:title" content="{title}">
<meta property="og:description" content="MTAP / PRMT5 axis: prevalence across 33 TCGA cohorts, focality of 9p21 deletions, co-deletion partners, clinical trial landscape, and synthesis. All claims cited.">
<meta property="og:type" content="article">
<meta name="twitter:card" content="summary">
<meta name="twitter:title" content="{title}">
<meta name="twitter:description" content="MTAP / PRMT5 axis: prevalence, focality, co-deletion partners, clinical landscape, and synthesis.">
<title>{title}</title>
<style>
  :root {{
    --teal-50:#E1F5EE;  --teal-100:#9FE1CB; --teal-200:#5DCAA5;
    --teal-400:#1D9E75; --teal-600:#0F6E56; --teal-800:#085041; --teal-900:#04342C;
    --gray-50:#F1EFE8;  --gray-100:#D3D1C7; --gray-400:#888780; --gray-900:#2C2C2A;
    --amber-50:#FAEEDA; --amber-100:#FAC775; --amber-600:#854F0B; --amber-900:#412402;
    --red-100:#F7C1C1;  --red-800:#791F1F;  --red-900:#501313;
    --blue-50:#E6F1FB;  --blue-100:#B5D4F4; --blue-600:#185FA5; --blue-800:#0C447C;
    --purple-100:#CECBF6; --purple-600:#534AB7; --purple-900:#26215C;
    --bg:#ffffff; --bg2:#fafaf7; --bg3:#f0efe9;
    --surface: #ffffff;
    --border:rgba(0,0,0,0.10); --border-strong:rgba(0,0,0,0.18);
    --text:#1a1a18; --text2:#5f5e5a; --text3:#888780;
    --shadow-sm: 0 1px 2px rgba(0,0,0,0.04);
    --shadow-md: 0 2px 12px rgba(0,0,0,0.06);
    --shadow-lg: 0 8px 28px rgba(0,0,0,0.12);
    --nav-h: 48px;
    --content-max: 1200px;
  }}
  * {{ box-sizing: border-box; }}
  html {{ scroll-behavior: smooth; }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI",
                       Inter, "Helvetica Neue", sans-serif;
          background: var(--bg); color: var(--text); font-size: 14.5px;
          line-height: 1.6; margin: 0; padding: 0;
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
          font-feature-settings: "ss01", "kern", "liga"; }}
  ::selection {{ background: var(--teal-100); color: var(--teal-900); }}

  /* Sticky nav — frosted, active-section highlight */
  nav.top {{ position: sticky; top: 0; z-index: 100;
             height: var(--nav-h);
             background: rgba(255,255,255,0.86);
             backdrop-filter: saturate(160%) blur(12px);
             -webkit-backdrop-filter: saturate(160%) blur(12px);
             border-bottom: 0.5px solid var(--border);
             padding: 0 28px; display: flex; align-items: center; gap: 18px; }}
  .brand {{ font-size: 12.5px; font-weight: 700; color: var(--text);
            white-space: nowrap; letter-spacing: 0.01em; }}
  .brand span {{ color: var(--teal-400); }}
  .navlinks {{ display: flex; gap: 0; overflow-x: auto;
                scrollbar-width: none; }}
  .navlinks::-webkit-scrollbar {{ display: none; }}
  .navlinks a {{ font-size: 11.5px; padding: 14px 14px; color: var(--text2);
                 border-bottom: 2px solid transparent; text-decoration: none;
                 white-space: nowrap; display: flex; align-items: center; gap: 6px;
                 transition: color 0.15s ease, border-color 0.15s ease;
                 font-weight: 500; }}
  .navlinks a:hover {{ color: var(--text); }}
  .navlinks a.active {{ color: var(--teal-600); border-bottom-color: var(--teal-400);
                          font-weight: 600; }}
  .navlinks .num {{ width: 17px; height: 17px; border-radius: 50%;
                    background: rgba(0,0,0,0.06); color: var(--text3);
                    font-size: 9px; font-weight: 700;
                    display: flex; align-items: center; justify-content: center;
                    transition: background 0.15s ease, color 0.15s ease; }}
  .navlinks a:hover .num,
  .navlinks a.active .num {{ background: var(--teal-400); color: #fff; }}
  .navlinks .ref-num {{ background: var(--blue-600); color: #fff; }}
  .navlinks a.active .ref-num {{ background: var(--blue-600); }}

  /* Header */
  header.db-h {{ padding: 32px 36px 22px; max-width: var(--content-max);
                 margin: 0 auto;
                 border-bottom: 0.5px solid var(--border); }}
  .tpill {{ display: inline-block; background: var(--teal-50); color: var(--teal-800);
            font-size: 10px; font-weight: 700; padding: 3px 10px; border-radius: 20px;
            margin-bottom: 8px; letter-spacing: 0.06em; text-transform: uppercase; }}
  h1 {{ font-size: 26px; margin: 0 0 6px; font-weight: 700;
        letter-spacing: -0.01em; line-height: 1.2; }}
  .meta {{ font-size: 11.5px; color: var(--text3); font-family: "SF Mono", monospace; }}

  /* Sections */
  section.section {{ padding: 28px 36px 40px; max-width: var(--content-max);
                      margin: 0 auto;
                      border-bottom: 0.5px solid var(--border);
                      scroll-margin-top: calc(var(--nav-h) + 8px); }}
  section.section:last-of-type {{ border-bottom: 0; }}
  .sec-h {{ display: flex; align-items: center; gap: 14px;
            margin: 0 -36px 22px; padding: 14px 36px;
            background: linear-gradient(180deg, var(--bg2) 0%, var(--bg) 100%);
            border-bottom: 0.5px solid var(--border);
            position: sticky; top: calc(var(--nav-h) - 1px); z-index: 50; }}
  .sec-num {{ width: 26px; height: 26px; border-radius: 50%;
              background: var(--teal-400); color: #fff;
              font-size: 11px; font-weight: 700;
              display: flex; align-items: center; justify-content: center;
              flex-shrink: 0;
              box-shadow: 0 1px 4px rgba(15,110,86,0.3); }}
  .ref-num {{ background: var(--blue-600);
              box-shadow: 0 1px 4px rgba(24,95,165,0.3); }}
  .sec-title {{ font-size: 15px; font-weight: 700; letter-spacing: -0.005em;
                color: var(--text); }}
  .sec-q {{ font-size: 11.5px; color: var(--text3); font-style: italic;
            margin-top: 2px; }}
  h3 {{ font-size: 15px; font-weight: 700; margin: 28px 0 12px;
        color: var(--text); letter-spacing: -0.005em;
        scroll-margin-top: calc(var(--nav-h) + 80px); }}
  h4 {{ font-size: 13px; font-weight: 700; margin: 18px 0 10px;
        color: var(--text); }}
  p, .lead {{ font-size: 13.5px; color: var(--text2); margin: 0 0 12px;
              line-height: 1.65; }}
  p b {{ color: var(--text); font-weight: 600; }}
  .lead {{ font-size: 13px; }}
  .aside {{ font-size: 12.5px; color: var(--text3); padding: 10px 14px;
             background: var(--bg2); border-radius: 8px;
             border-left: 3px solid var(--gray-100); margin: 14px 0; }}
  .aside code {{ font-family: "SF Mono", monospace; font-size: 11.5px;
                  background: var(--bg); padding: 1px 4px; border-radius: 3px;
                  border: 0.5px solid var(--border); }}

  /* Two-up figure grid (Figure 1 + 2 ; Figure 3 + 4) */
  .fig-pair {{ display: grid; grid-template-columns: 1fr 1fr; gap: 22px;
              margin: 18px 0; align-items: start; }}
  .fig-pair-col {{ min-width: 0; }}
  .fig-pair-col h3 {{ margin-top: 0; }}
  .fig-pair .fig-wrap {{ margin: 8px 0 0; }}
  @media (max-width: 1100px) {{
    .fig-pair {{ grid-template-columns: 1fr; gap: 12px; }}
  }}

  /* Figure / image sizing — keep visuals from dominating full-page view */
  .fig-wrap {{ margin: 16px auto; max-width: 920px; padding: 0; }}
  /* Fade-in is JS-driven (IntersectionObserver). Without JS, content
     must remain visible — scope the initial opacity: 0 to .js only. */
  html.js .fig-wrap {{ opacity: 0; transform: translateY(14px);
                        transition: opacity 0.7s ease, transform 0.7s ease; }}
  /* Full-width variant: Figure 1 + 2 take the full section width so the
     headline figures are easier to read. */
  .fig-wrap.fig-full {{ max-width: 100%; }}
  .fig-wrap.fig-full svg.figure-svg,
  .fig-wrap.fig-full svg.stage1-svg {{ max-width: 100%; }}
  .fig-wrap.visible {{ opacity: 1; transform: translateY(0); }}
  .fig-cap {{ font-size: 11px; color: var(--text3); margin-top: 6px;
              text-align: center; line-height: 1.45; }}
  img.fig {{ max-width: 880px; width: 100%; height: auto;
              border: 0.5px solid var(--border);
              border-radius: 8px; margin: 6px auto 6px;
              background: white; display: block;
              box-shadow: 0 1px 4px rgba(0,0,0,0.04); }}
  html.js img.fig {{ opacity: 0; transform: translateY(14px);
                      transition: opacity 0.7s ease, transform 0.7s ease; }}
  img.fig.visible {{ opacity: 1; transform: translateY(0); }}

  /* Shared figure SVG */
  svg.figure-svg, svg.stage1-svg {{ width: 100%; height: auto; max-width: 920px;
                                     display: block; margin: 0 auto;
                                     font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }}
  .fig-axis-x, .fig-axis-y {{ font-size: 10px; fill: var(--text2); }}
  .fig-legend-note {{ font-size: 10px; fill: var(--text3); }}

  /* Figure 2 (CN→expression mini-panels) */
  .fig2-svg .f2-bg     {{ fill: var(--bg2); stroke: var(--border); stroke-width: 0.5;
                          transition: fill 0.18s ease; }}
  .fig2-svg .f2-panel:hover .f2-bg {{ fill: #F0FAF6; }}
  .fig2-svg .f2-title  {{ font-size: 9.5px; fill: var(--text); text-anchor: middle;
                          font-weight: 600; }}
  .fig2-svg .f2-xtick  {{ font-size: 8.5px; fill: var(--text3); text-anchor: middle; }}
  .fig2-svg .f2-axis   {{ stroke: var(--border); stroke-width: 0.5; }}
  .fig2-svg .f2-line   {{ stroke: var(--teal-600); stroke-width: 1.5; fill: none;
                          stroke-dasharray: 600;
                          stroke-dashoffset: 600;
                          transition: stroke-dashoffset 0.9s ease 0.2s; }}
  .fig2-svg.visible .f2-line {{ stroke-dashoffset: 0; }}
  .fig2-svg .f2-dot    {{ stroke: #fff; stroke-width: 1;
                          opacity: 0; transform-origin: center;
                          transition: opacity 0.5s ease, r 0.5s ease;
                          transition-delay: 0.5s; }}
  .fig2-svg.visible .f2-dot {{ opacity: 1; }}
  .fig2-svg .f2-cn0    {{ fill: var(--teal-600); }}
  .fig2-svg .f2-cn1    {{ fill: var(--teal-400); }}
  .fig2-svg .f2-cn2    {{ fill: var(--gray-100); }}
  .fig2-svg .f2-cn3    {{ fill: var(--amber-100); }}

  /* Figure 3 (focality) */
  .fig3-svg .cohort-label {{ font-size: 10px; fill: var(--text);
                              font-family: "SF Mono", monospace;
                              font-weight: 600; text-anchor: end; }}
  .fig3-svg .f3-facet-title {{ font-size: 11.5px; fill: var(--text);
                                font-weight: 700; }}
  .fig3-svg .f3-row-label   {{ font-size: 10px; fill: var(--text2);
                                font-family: "SF Mono", monospace;
                                opacity: 0; transition: opacity 0.6s ease 0.5s; }}
  .fig3-svg.visible .f3-row-label {{ opacity: 1; }}
  .fig3-svg .f3-seg     {{ transition: width 0.8s cubic-bezier(.4,1.4,.5,1), x 0.8s cubic-bezier(.4,1.4,.5,1); }}
  /* Monochromatic teal-to-neutral sequential palette.  Reads as a single
     gradient: saturated teal for focal (the signal worth highlighting),
     light teal for intermediate, muted neutral for arm-level (passenger,
     visually quieter). */
  .fig3-svg .f3-focal   {{ fill: var(--teal-800); }}    /* deep teal, driver-like */
  .fig3-svg .f3-inter   {{ fill: var(--teal-200); }}    /* light teal middle */
  .fig3-svg .f3-arm     {{ fill: var(--gray-100);
                            stroke: rgba(0,0,0,0.08); stroke-width: 0.5; }}    /* neutral, passenger-like */
  /* Legend swatch needs a stronger outline so the light gray-100 fill
     is still visible against the white page background. */
  .fig3-svg .f3-arm-legend {{ stroke: var(--gray-400); stroke-width: 1; }}
  .fig3-svg .f3-hit     {{ fill: transparent; }}
  .fig3-svg .f3-row:hover .f3-hit {{ fill: rgba(15,110,86,0.05); }}
  .fig3-svg .f3-nodata  {{ font-size: 10px; fill: var(--text3); text-anchor: middle;
                            font-style: italic; }}

  /* Figure 4 (heatmap) */
  .fig4-svg .f4-cell    {{ stroke: #fff; stroke-width: 1;
                            transition: filter 0.15s ease; }}
  .fig4-svg .f4-cell-g:hover .f4-cell {{ filter: brightness(1.1) drop-shadow(0 1px 3px rgba(0,0,0,0.18)); }}
  .fig4-svg .f4-na      {{ fill: var(--bg3); stroke: #fff; }}
  .fig4-svg .f4-label   {{ font-size: 9.5px; text-anchor: middle; pointer-events: none;
                            font-family: "SF Mono", monospace; font-weight: 600; }}
  .fig4-svg .f4-xlab    {{ font-size: 9.5px; fill: var(--text); font-family: "SF Mono", monospace; }}
  .fig4-svg .f4-ylab    {{ font-size: 10.5px; fill: var(--text); font-family: "SF Mono", monospace;
                            font-weight: 600; text-anchor: end; }}
  .fig4-svg .f4-sep     {{ stroke: var(--gray-400); stroke-width: 0.8;
                            stroke-dasharray: 3 3; }}
  .fig4-svg .legend-tick {{ font-size: 8.5px; fill: var(--text3); text-anchor: middle; }}

  /* Figure 5 (lollipops) */
  .fig5-svg .f5-bg      {{ fill: var(--bg2); stroke: var(--border); stroke-width: 0.5;
                            transition: fill 0.18s ease; }}
  .fig5-svg .f5-panel:hover .f5-bg {{ fill: #F0FAF6; }}
  .fig5-svg .f5-title   {{ font-size: 11px; fill: var(--text); font-weight: 700; }}
  .fig5-svg .f5-gene    {{ font-size: 10px; fill: var(--text);
                            font-family: "SF Mono", monospace; text-anchor: end; }}
  .fig5-svg .f5-tick    {{ stroke: var(--border); stroke-width: 0.4; stroke-dasharray: 2 3; }}
  .fig5-svg .f5-ticklbl {{ font-size: 9px; fill: var(--text3); text-anchor: middle; }}
  .fig5-svg .f5-baseline {{ fill: #fff; stroke: var(--gray-400); stroke-width: 1; }}
  .fig5-svg .f5-homdel  {{ fill: var(--teal-600); }}
  .fig5-svg .f5-link    {{ stroke: var(--gray-400); stroke-width: 1.2;
                            stroke-dasharray: 800; stroke-dashoffset: 800;
                            transition: stroke-dashoffset 0.7s ease 0.3s; }}
  .fig5-svg.visible .f5-link {{ stroke-dashoffset: 0; }}
  .fig5-svg .f5-hit     {{ fill: transparent; }}
  .fig5-svg .f5-row:hover .f5-hit {{ fill: rgba(15,110,86,0.06); }}

  /* Figure 6 (Gantt) */
  .fig6-svg .f6-grid    {{ stroke: var(--border); stroke-width: 0.4; stroke-dasharray: 2 4; }}
  .fig6-svg .f6-yrlbl   {{ font-size: 10px; fill: var(--text3); text-anchor: middle; }}
  .fig6-svg .f6-drug    {{ font-size: 10px; fill: var(--text);
                            font-family: "SF Mono", monospace; font-weight: 600; text-anchor: end; }}
  .fig6-svg .f6-nct     {{ font-size: 9.5px; fill: var(--blue-600);
                            font-family: "SF Mono", monospace; text-anchor: end; }}
  .fig6-svg .f6-bar     {{ transition: width 0.8s cubic-bezier(.4,1.4,.5,1); }}
  .fig6-svg .f6-row:hover .f6-bar {{ filter: brightness(0.9); }}
  .fig6-svg .f6-hit     {{ fill: transparent; cursor: pointer; }}
  .fig6-svg .f6-row:hover .f6-hit {{ fill: rgba(15,110,86,0.04); }}
  .fig6-svg .f6-marker  {{ font-size: 11px; fill: var(--text); text-anchor: middle; }}

  /* Original Stage-1 styles still apply via svg.stage1-svg */
  svg.stage1-svg {{ }}

  /* ---- Filterable trial table -------------------------------------- */
  .trial-table-wrap {{ background: var(--bg2); border: 0.5px solid var(--border);
                        border-radius: 10px; padding: 12px; margin-bottom: 14px; }}
  .filters {{ display: flex; flex-wrap: wrap; gap: 8px; align-items: center;
              margin-bottom: 10px; font-size: 12px; }}
  .filter-search {{ flex: 1 1 240px; display: flex; align-items: center;
                     background: var(--bg); border: 0.5px solid var(--border);
                     border-radius: 6px; padding: 0 8px; }}
  .filter-search span {{ font-size: 12px; color: var(--text3); margin-right: 4px; }}
  .filter-search input {{ flex: 1; border: 0; outline: 0; background: transparent;
                           font-size: 12px; padding: 6px 4px; color: var(--text); }}
  .filter-select {{ background: var(--bg); border: 0.5px solid var(--border);
                     border-radius: 6px; padding: 5px 8px; font-size: 11px;
                     color: var(--text); cursor: pointer; }}
  .filter-toggle {{ display: flex; align-items: center; gap: 5px;
                     color: var(--text2); font-size: 11px; cursor: pointer; }}
  .filter-clear {{ background: var(--bg); border: 0.5px solid var(--border);
                    border-radius: 6px; padding: 5px 10px; font-size: 11px;
                    color: var(--text2); cursor: pointer; }}
  .filter-clear:hover {{ background: var(--bg3); }}
  .tt-count {{ margin-left: auto; font-size: 11px; color: var(--text3);
                font-family: "SF Mono", monospace; }}

  .tt-scroll {{ overflow-x: auto; max-height: 720px; overflow-y: auto;
                border: 0.5px solid var(--border); border-radius: 8px;
                background: var(--bg); }}
  table.trial-table {{ width: 100%; border-collapse: collapse; font-size: 11.5px; }}
  table.trial-table th {{ position: sticky; top: 0; z-index: 2;
                            text-align: left; padding: 8px 10px;
                            background: var(--bg2); border-bottom: 0.5px solid var(--border);
                            font-size: 10px; font-weight: 700; color: var(--text);
                            text-transform: uppercase; letter-spacing: 0.04em;
                            white-space: nowrap; user-select: none; }}
  table.trial-table th.tt-sort {{ cursor: pointer; }}
  table.trial-table th.tt-sort:hover {{ color: var(--teal-600); }}
  table.trial-table th .sort-ind {{ display: inline-block; width: 10px;
                                    margin-left: 3px; color: var(--text3);
                                    font-size: 9px; }}
  table.trial-table th.tt-sort-asc  .sort-ind::before {{ content: "▲"; color: var(--teal-600); }}
  table.trial-table th.tt-sort-desc .sort-ind::before {{ content: "▼"; color: var(--teal-600); }}
  /* Mechanism pill */
  .pill.mech-mta   {{ background: var(--teal-100); color: var(--teal-900); }}
  .pill.mech-mat2a {{ background: var(--purple-100); color: var(--purple-900); }}
  .pill.mech-fg    {{ background: var(--gray-100); color: var(--gray-900); }}
  td.tt-orr {{ font-family: "SF Mono", monospace; font-size: 11.5px;
                font-weight: 700; color: var(--teal-800); }}
  td.tt-n   {{ font-family: "SF Mono", monospace; font-size: 11px;
                color: var(--text2); white-space: nowrap; }}
  td.tt-n .n-ctx {{ color: var(--text3); font-size: 10.5px;
                     font-family: -apple-system, sans-serif; font-weight: 500; }}
  table.trial-table td {{ padding: 7px 10px; border-bottom: 0.5px solid var(--border);
                            vertical-align: top; color: var(--text); }}
  table.trial-table tr.tt-row {{ cursor: pointer; transition: background 0.12s ease; }}
  table.trial-table tr.tt-row:hover {{ background: rgba(15,110,86,0.05); }}
  table.trial-table tr.tt-row.tt-active {{ background: rgba(15,110,86,0.07); }}
  .tt-nct a {{ color: var(--blue-600); text-decoration: none; font-weight: 600;
                font-family: "SF Mono", monospace; }}
  .tt-nct a:hover {{ text-decoration: underline; }}
  .tt-drug b {{ font-size: 12px; }}
  .tt-sponsor {{ font-size: 10px; color: var(--text3); margin-top: 1px; }}
  .tt-indication {{ font-size: 11px; max-width: 200px; }}
  .tt-date {{ font-family: "SF Mono", monospace; font-size: 10px; color: var(--text2); }}
  .tt-results .results-toggle {{ background: var(--teal-100); color: var(--teal-900);
                                   border: 0; padding: 3px 8px; border-radius: 12px;
                                   font-size: 10px; font-weight: 600; cursor: pointer; }}
  .tt-results .results-toggle:hover {{ background: var(--teal-200); }}
  .tt-results .results-toggle[aria-expanded="true"] {{ background: var(--teal-400); color: #fff; }}
  .tt-results .results-link {{ font-size: 10px; color: var(--blue-600);
                                text-decoration: none; padding: 3px 8px;
                                background: var(--blue-100); border-radius: 12px;
                                font-weight: 600; }}
  .tt-results .results-none {{ color: var(--text3); }}

  tr.tt-detail-row td {{ padding: 0; background: var(--bg2);
                          border-bottom: 0.5px solid var(--border); }}
  .tt-detail-body {{ padding: 12px 16px; font-size: 12px;
                      animation: detail-in 0.18s ease; }}
  @keyframes detail-in {{
    from {{ opacity: 0; transform: translateY(-4px); }}
    to   {{ opacity: 1; transform: translateY(0); }}
  }}
  .tt-detail-body .trial-detail-row {{ margin: 6px 0; color: var(--text2); }}
  .tt-detail-body .trial-detail-row b {{ color: var(--text); }}
  .tt-detail-body .elig-body {{ margin-top: 4px; padding: 8px 10px;
                                  background: var(--bg); border-radius: 5px;
                                  white-space: pre-wrap; font-size: 11px; }}
  .tt-detail-body .readout {{ background: #F0FAF6; border-left: 3px solid var(--teal-400);
                                 border-radius: 0 6px 6px 0; padding: 8px 12px;
                                 margin-bottom: 8px; }}
  .tt-detail-body .readout-head {{ font-size: 12px; font-weight: 600;
                                     color: var(--teal-900); margin-bottom: 4px; }}
  .tt-detail-body .readout ul {{ margin: 4px 0 0; padding-left: 18px; font-size: 12px; }}
  .stage1-svg .axis-baseline {{ stroke: var(--border); stroke-width: 0.6; }}
  .stage1-svg .axis-tick     {{ stroke: var(--border); stroke-width: 0.5;
                                 stroke-dasharray: 2 3; }}
  .stage1-svg .axis-label    {{ font-size: 9px; fill: var(--text3);
                                 text-anchor: middle; }}
  .stage1-svg .axis-title    {{ font-size: 10px; fill: var(--text2); }}
  .stage1-svg .cohort-label  {{ font-size: 10px; fill: var(--text); font-weight: 600;
                                 text-anchor: end; font-family: "SF Mono", monospace; }}
  .stage1-svg .row-pct       {{ font-size: 10px; fill: var(--text2);
                                 font-family: "SF Mono", monospace; }}
  html.js .stage1-svg .row-pct {{ opacity: 0; transition: opacity 0.6s ease 0.5s; }}
  .stage1-svg.visible .row-pct {{ opacity: 1; }}
  .stage1-svg .bar           {{ transition: width 0.8s cubic-bezier(.4,1.4,.5,1);
                                 transform-origin: left center; cursor: default; }}
  .stage1-svg .bar-hom       {{ fill: var(--teal-600); }}
  .stage1-svg .bar-het       {{ fill: var(--teal-100); }}
  .stage1-svg .row-hit       {{ fill: transparent; cursor: default; }}
  .stage1-svg .bar-row:hover .row-hit {{ fill: rgba(15,110,86,0.045); }}
  .stage1-svg .bar-row:hover .bar-hom {{ fill: var(--teal-800); }}
  .stage1-svg .bar-row:hover .bar-het {{ fill: var(--teal-200); }}
  .stage1-svg .legend-text   {{ font-size: 10px; fill: var(--text2); }}

  /* Tooltip */
  .tooltip {{ position: fixed; z-index: 1000; pointer-events: none;
               background: rgba(28,28,26,0.95); color: #fff;
               padding: 8px 12px; border-radius: 6px;
               font-size: 11px; line-height: 1.5;
               box-shadow: 0 4px 16px rgba(0,0,0,0.18);
               opacity: 0; transform: translateY(4px);
               transition: opacity 0.15s ease, transform 0.15s ease;
               max-width: 240px; }}
  .tooltip.visible {{ opacity: 1; transform: translateY(0); }}
  .tooltip .tt-title {{ font-weight: 700; font-family: "SF Mono", monospace;
                         color: #fff; margin-bottom: 2px; }}
  .tooltip .tt-row   {{ display: flex; justify-content: space-between;
                         gap: 12px; font-size: 11px; }}
  .tooltip .tt-row b {{ color: #9FE1CB; }}
  .tooltip .tt-sub   {{ font-size: 10px; color: rgba(255,255,255,0.55);
                         margin-top: 3px; }}

  /* Scroll-triggered fade-in for sections (JS-driven; only initialise when
     JS is confirmed available so non-JS previews still render the content). */
  html.js section.section {{ opacity: 0; transform: translateY(10px);
                              transition: opacity 0.6s ease, transform 0.6s ease; }}
  section.section.visible {{ opacity: 1; transform: translateY(0); }}

  /* Card + figure entrance refinements */
  .card, .indication, .prog, .thesis, ul.bm li, ul.ps li, ul.risk li,
  ul.steps li, ul.uncert li, .step, .strat, details.drug, details.trial {{
    transition: box-shadow 0.18s ease, transform 0.18s ease, border-color 0.18s ease;
  }}
  .card:hover, .indication:hover, .prog:hover {{
    box-shadow: 0 2px 10px rgba(0,0,0,0.06);
    transform: translateY(-1px);
  }}

  /* Grid + cards */
  .grid-2 {{ display: grid; grid-template-columns: 1fr 1fr; gap: 12px;
            margin-bottom: 14px; }}
  .card {{ background: var(--surface); border-radius: 10px;
           border: 0.5px solid var(--border); overflow: hidden;
           box-shadow: var(--shadow-sm);
           transition: box-shadow 0.18s ease, transform 0.18s ease;
           min-width: 0; }}
  .card:hover {{ box-shadow: var(--shadow-md); transform: translateY(-1px); }}
  .card-h {{ font-size: 11.5px; font-weight: 700; padding: 10px 14px;
             background: var(--teal-50); color: var(--teal-900);
             border-bottom: 0.5px solid var(--border);
             letter-spacing: 0.005em; }}
  .card-b {{ padding: 10px 14px; }}
  .cohort-list {{ list-style: none; padding: 0; margin: 0 0 10px;
                   font-size: 12px; }}
  .cohort-list li {{ display: flex; gap: 10px; padding: 4px 0;
                      border-bottom: 0.5px solid var(--border); }}
  .cohort-list li:last-child {{ border-bottom: 0; }}
  .cohort {{ font-family: "SF Mono", monospace; font-weight: 700;
             min-width: 52px; color: var(--teal-600); font-size: 11.5px; }}
  .card-foot {{ font-size: 11px; color: var(--text3); padding-top: 8px;
                 border-top: 0.5px solid var(--border); margin-top: 4px;
                 line-height: 1.6; }}
  .card-foot b {{ color: var(--text2); }}

  .prev-card {{ width: 100%; max-width: none; margin-bottom: 12px; }}
  p.msk-note {{ font-size: 12.5px; color: var(--text2);
                background: var(--bg2); border-left: 3px solid var(--teal-200);
                border-radius: 0 8px 8px 0; padding: 10px 14px;
                margin: 4px 0 14px; line-height: 1.6; }}
  p.msk-note b {{ color: var(--text); }}

  .callout {{ background: var(--amber-50); color: var(--amber-900);
              border-left: 3px solid var(--amber-600); border-radius: 0 8px 8px 0;
              padding: 12px 16px; font-size: 12.5px; margin: 12px 0 18px;
              line-height: 1.6; }}
  .callout b {{ color: var(--amber-900); }}

  /* Mechanism flow */
  .flow {{ display: grid; gap: 8px; margin-bottom: 14px; }}
  .step {{ display: grid; grid-template-columns: 26px 1fr; gap: 10px;
           padding: 11px 13px; background: var(--bg2); border-radius: 8px;
           border: 0.5px solid var(--border); }}
  .step-num {{ width: 22px; height: 22px; border-radius: 50%;
               background: var(--teal-400); color: #fff;
               font-size: 10px; font-weight: 700;
               display: flex; align-items: center; justify-content: center; }}
  .step-label {{ font-size: 10px; font-weight: 700; color: var(--text2);
                 text-transform: uppercase; letter-spacing: 0.05em;
                 margin-bottom: 3px; }}
  .step-text {{ font-size: 13px; }}

  ul.bm, ul.uncert, ul.ps, ul.risk, ul.steps {{ list-style: none;
                                               padding: 0; margin: 0; }}
  ul.bm li {{ padding: 9px 13px; border-left: 3px solid var(--teal-400);
              background: var(--bg2); border-radius: 0 6px 6px 0;
              margin-bottom: 7px; }}
  .bm-point {{ font-weight: 600; font-size: 13px; margin-bottom: 3px; }}
  .bm-detail {{ font-size: 12px; color: var(--text2); }}
  .strat {{ padding: 9px 13px; background: var(--bg2); border-radius: 8px;
            margin-bottom: 7px; border-left: 3px solid var(--gray-400); }}
  .strat-class {{ font-weight: 700; font-size: 12px; margin-bottom: 3px; }}
  .strat-text {{ font-size: 12px; color: var(--text2); }}
  ul.uncert li {{ padding: 8px 13px; background: var(--amber-50); color: var(--amber-900);
                   border-radius: 6px; margin-bottom: 6px; font-size: 12px; }}

  /* Clinical landscape */
  .legend {{ font-size: 11px; color: var(--text2); margin: 0 0 14px;
              display: flex; gap: 8px; flex-wrap: wrap; align-items: center; }}
  .class h4 {{ color: var(--text); }}
  details.drug {{ background: var(--bg2); border-radius: 9px; padding: 0;
                  border: 0.5px solid var(--border); margin-bottom: 10px;
                  overflow: hidden; }}
  details.drug > summary {{ padding: 11px 15px; cursor: pointer; list-style: none;
                            display: flex; flex-wrap: wrap; align-items: baseline;
                            gap: 8px; background: var(--bg2); }}
  details.drug > summary::-webkit-details-marker {{ display: none; }}
  details.drug > summary::before {{ content: "▶"; transform: rotate(0); font-size: 9px;
                                    color: var(--text3); margin-right: 6px; }}
  details.drug[open] > summary::before {{ content: "▼"; }}
  .drug-name {{ font-size: 13px; font-weight: 700; }}
  .alias {{ font-size: 10px; color: var(--text3); }}
  .mech  {{ font-size: 10px; color: var(--text3); margin-left: auto; }}
  .rollup {{ width: 100%; font-size: 11px; color: var(--text2); }}
  .rollup .st-active     {{ color: var(--teal-600); }}
  .rollup .st-planned    {{ color: var(--amber-600); }}
  .rollup .st-completed  {{ color: var(--blue-600); }}
  .rollup .st-terminated {{ color: var(--red-800); }}
  .drug-body {{ padding: 8px 14px 14px; background: var(--bg); border-top: 0.5px solid var(--border); }}
  .notes {{ font-size: 11px; color: var(--text2); font-style: italic;
            margin-bottom: 8px; padding: 5px 9px; background: var(--bg2);
            border-radius: 5px; }}

  details.trial {{ background: var(--bg); border: 0.5px solid var(--border);
                   border-radius: 6px; margin-bottom: 6px; }}
  details.trial > summary {{ padding: 7px 11px; cursor: pointer; list-style: none;
                             display: grid; gap: 3px; }}
  details.trial > summary::-webkit-details-marker {{ display: none; }}
  details.trial > summary::before {{ content: "▸"; color: var(--text3);
                                     font-size: 9px; margin-right: 3px; }}
  details.trial[open] > summary::before {{ content: "▾"; }}
  details.trial[open] > summary {{ border-bottom: 0.5px solid var(--border);
                                   background: var(--bg2); }}
  .trial-head {{ display: flex; flex-wrap: wrap; gap: 5px; align-items: center; }}
  .trial-title {{ font-size: 12px; }}
  .trial-meta  {{ font-size: 10px; color: var(--text3); font-family: "SF Mono", monospace; }}
  .nct {{ color: var(--blue-600); text-decoration: none; font-weight: 600;
          font-family: "SF Mono", monospace; font-size: 11px;
          padding: 1px 6px; background: var(--blue-100); border-radius: 4px; }}
  .nct:hover {{ text-decoration: underline; }}
  .trial-body {{ padding: 9px 13px; font-size: 12px; }}
  .trial-body > div, .trial-body details {{ margin-bottom: 5px; }}
  .brief {{ color: var(--text2); }}
  .endpoint, .interv {{ color: var(--text2); }}
  .endpoint b, .interv b {{ color: var(--text); }}
  details.elig {{ background: var(--bg2); border-radius: 5px;
                  padding: 4px 8px; font-size: 11px; }}
  details.elig > summary {{ cursor: pointer; list-style: none; color: var(--text2);
                            font-weight: 500; }}
  details.elig > summary::-webkit-details-marker {{ display: none; }}
  details.elig > summary::before {{ content: "▸ "; color: var(--text3); }}
  details.elig[open] > summary::before {{ content: "▾ "; }}
  .elig-body {{ padding: 6px 0; color: var(--text2); white-space: pre-wrap;
                font-family: -apple-system, sans-serif; }}
  .readout {{ background: #F0FAF6; border-left: 3px solid var(--teal-400);
              border-radius: 0 6px 6px 0; padding: 7px 11px; margin-bottom: 7px; }}
  .readout-head {{ font-size: 12px; font-weight: 600; color: var(--teal-900);
                   margin-bottom: 3px; }}
  .readout ul {{ margin: 3px 0 0; padding-left: 18px; font-size: 12px; }}
  .readout li {{ margin-bottom: 2px; }}

  /* Pills */
  .pill {{ display: inline-block; font-size: 9px; font-weight: 600;
           padding: 2px 7px; border-radius: 10px; white-space: nowrap;
           letter-spacing: 0.02em; }}
  .ph-ph1 {{ background: var(--teal-100); color: var(--teal-900); }}
  .ph-ph12 {{ background: var(--teal-400); color: #fff; }}
  .ph-ph2 {{ background: var(--teal-600); color: #fff; }}
  .ph-ph23 {{ background: var(--teal-900); color: #fff; }}
  .ph-ph3 {{ background: #000; color: #fff; }}
  .ph-unknown {{ background: var(--gray-100); color: var(--gray-900); }}
  .st-active     {{ background: var(--teal-100); color: var(--teal-900); }}
  .st-planned    {{ background: var(--amber-100); color: var(--amber-600); }}
  .st-completed  {{ background: var(--blue-100); color: var(--blue-600); }}
  .st-terminated {{ background: var(--red-100); color: var(--red-800); }}
  .bm-strict {{ background: var(--amber-50); color: var(--amber-600); }}
  .bm-soft   {{ background: var(--gray-100); color: var(--gray-900); }}
  .bm-none   {{ background: #FCEBEB; color: var(--red-800); }}
  .cb-mono   {{ background: var(--bg3); color: var(--text2); }}
  .cb-chemo  {{ background: var(--gray-100); color: var(--gray-900); }}
  .cb-kras   {{ background: var(--purple-100); color: var(--purple-600); }}
  .cb-egfr   {{ background: var(--blue-100); color: var(--blue-600); }}
  .cb-io     {{ background: var(--teal-100); color: var(--teal-900); }}
  .cb-parp   {{ background: #FCEBEB; color: var(--red-800); }}
  .cb-other  {{ background: var(--gray-100); color: var(--gray-900); }}

  /* Synthesis */
  .thesis {{ background: linear-gradient(180deg, #F0FAF6 0%, var(--bg2) 100%);
              border: 0.5px solid var(--teal-100); border-radius: 10px;
              padding: 14px 16px; margin-bottom: 12px; font-size: 13px;
              line-height: 1.65; }}
  .thesis .refs {{ font-size: 11px; color: var(--text3); margin-top: 7px; }}
  ul.ps li {{ padding: 10px 13px; border-left: 3px solid var(--teal-400);
              background: var(--bg2); border-radius: 0 6px 6px 0; margin-bottom: 9px; }}
  ul.risk li {{ padding: 10px 13px; border-left: 3px solid var(--amber-600);
                 background: var(--amber-50); border-radius: 0 6px 6px 0;
                 margin-bottom: 9px; }}
  ul.steps li {{ padding: 10px 13px; border-left: 3px solid var(--purple-600);
                 background: #EEEDFE; border-radius: 0 6px 6px 0;
                 margin-bottom: 9px; }}
  .ps-point, .risk-point, .step-act {{ font-weight: 700; margin-bottom: 3px; }}
  .ps-detail, .risk-detail, .step-why {{ font-size: 12px; color: var(--text2); }}
  .ps-refs, .risk-refs, .ind-refs {{ font-size: 10px; color: var(--text3); margin-top: 5px; }}
  .indication {{ background: var(--bg2); border-radius: 8px;
                  padding: 10px 13px; margin-bottom: 9px;
                  border: 0.5px solid var(--border); }}
  .ind-head {{ display: flex; gap: 10px; align-items: baseline; margin-bottom: 3px; }}
  .ind-name {{ font-weight: 700; font-size: 13px; }}
  .ind-prev {{ font-size: 10px; color: var(--text3); font-family: "SF Mono", monospace; }}
  .strats {{ margin: 5px 0; padding-left: 18px; }}
  .strats li {{ font-size: 12px; }}
  .ind-rat {{ font-size: 12px; color: var(--text2); padding: 5px 9px;
              background: var(--bg); border-radius: 5px; }}
  .prog {{ background: var(--bg2); border-radius: 8px; padding: 9px 13px;
            margin-bottom: 7px; border-left: 3px solid var(--blue-600); }}
  .prog-drug {{ font-family: "SF Mono", monospace; color: var(--blue-600); }}
  .prog-pos {{ font-size: 12px; color: var(--text2); margin: 2px 0; }}
  .prog-cat {{ font-size: 11px; color: var(--text2); }}

  /* Cite chip + panel ref */
  sup.cites {{ font-size: 9px; line-height: 1; margin-left: 3px;
                white-space: nowrap; }}
  a.cite {{ color: var(--teal-600); text-decoration: none; padding: 1px 5px;
            border: 0.5px solid var(--teal-100); border-radius: 4px;
            background: #F0FAF6; font-size: 9.5px; font-weight: 700;
            transition: background 0.12s ease, transform 0.12s ease;
            margin-right: 1px;
            display: inline-block; vertical-align: middle; }}
  a.cite:hover {{ background: var(--teal-100); transform: translateY(-1px); }}
  .cite.missing {{ color: #A32D2D; background: #FCEBEB; border-color: #F7C1C1; }}
  a.panel-ref {{ display: inline-block; font-size: 9.5px; font-weight: 700;
                  padding: 1px 6px; background: var(--purple-100);
                  color: var(--purple-900); border-radius: 4px;
                  text-decoration: none; margin-right: 3px;
                  transition: background 0.12s ease, color 0.12s ease; }}
  a.panel-ref:hover {{ background: var(--purple-600); color: #fff; }}

  /* References + audit */
  ol.refs {{ font-size: 12px; color: var(--text2); padding-left: 24px;
              line-height: 1.6; }}
  ol.refs li {{ margin-bottom: 9px;
                  scroll-margin-top: calc(var(--nav-h) + 80px);
                  transition: background 0.3s ease, padding 0.3s ease; }}
  ol.refs li:target {{ background: var(--teal-50); padding: 6px 8px;
                       border-radius: 6px;
                       box-shadow: 0 0 0 2px var(--teal-100);
                       animation: ref-pulse 1.4s ease-out; }}
  @keyframes ref-pulse {{
    0% {{ box-shadow: 0 0 0 6px var(--teal-100); }}
    100% {{ box-shadow: 0 0 0 2px var(--teal-100); }}
  }}
  .ref-authors {{ font-weight: 600; color: var(--text); }}
  .ref-title  {{ color: var(--text); margin-left: 4px; }}
  .ref-link   {{ margin-left: 8px; color: var(--teal-600);
                  text-decoration: none; font-weight: 600; font-size: 10.5px;
                  padding: 1px 6px; background: #F0FAF6;
                  border: 0.5px solid var(--teal-100); border-radius: 4px; }}
  .ref-link:hover {{ background: var(--teal-100); }}

  .audit-wrap {{ overflow-x: auto; border: 0.5px solid var(--border);
                  border-radius: 8px; background: var(--surface);
                  box-shadow: var(--shadow-sm); }}
  table.audit {{ width: 100%; border-collapse: collapse; font-size: 11.5px; }}
  table.audit th {{ text-align: left; padding: 8px 10px;
                    border-bottom: 0.5px solid var(--border);
                    background: var(--bg2); font-size: 9.5px;
                    text-transform: uppercase; color: var(--text3);
                    letter-spacing: 0.05em; font-weight: 700;
                    position: sticky; top: 0; }}
  table.audit td {{ padding: 6px 10px; border-bottom: 0.5px solid var(--border); }}
  table.audit tr:last-child td {{ border-bottom: 0; }}
  table.audit tr:hover td {{ background: var(--bg2); }}
  table.audit td.mono {{ font-family: "SF Mono", monospace; font-size: 10.5px;
                          color: var(--text3); }}

  .empty {{ font-size: 11px; color: var(--text3); font-weight: 400; }}
  .missing-img {{ color: var(--red-800); font-style: italic; font-size: 11px;
                  padding: 8px 12px; background: #FCEBEB; border-radius: 5px; }}

  /* Scroll-progress indicator */
  #scroll-progress {{ position: fixed; top: 0; left: 0; height: 2px;
                       background: linear-gradient(90deg, var(--teal-400),
                                                   var(--teal-600));
                       width: 0%; z-index: 200;
                       transition: width 0.06s linear; }}

  /* Back-to-top */
  #back-top {{ position: fixed; right: 24px; bottom: 24px; z-index: 150;
                width: 40px; height: 40px; border-radius: 20px;
                background: var(--surface); color: var(--text);
                border: 0.5px solid var(--border-strong);
                box-shadow: var(--shadow-md);
                display: flex; align-items: center; justify-content: center;
                cursor: pointer; font-size: 16px;
                opacity: 0; pointer-events: none;
                transition: opacity 0.25s ease, transform 0.18s ease;
                font-family: inherit; }}
  #back-top.visible {{ opacity: 1; pointer-events: auto; }}
  #back-top:hover {{ background: var(--teal-50); border-color: var(--teal-200);
                       transform: translateY(-2px); }}

  /* Reduced motion */
  @media (prefers-reduced-motion: reduce) {{
    *, *::before, *::after {{ transition: none !important; animation: none !important; }}
    html {{ scroll-behavior: auto; }}
  }}

  /* Print */
  @media print {{
    nav.top, #scroll-progress, #back-top, #tt {{ display: none !important; }}
    section.section {{ break-after: page; border: 0; }}
    .sec-h {{ position: static !important; background: #fff !important; }}
    body {{ font-size: 10.5pt; line-height: 1.45; color: #000; }}
    .fig-wrap, img.fig, svg.figure-svg {{ break-inside: avoid; }}
    a {{ color: #000 !important; }}
    a.cite {{ border: 0; padding: 0; background: transparent; color: #000; }}
    a.cite::after {{ content: " (" attr(href) ")"; font-size: 8pt; color: #444; }}
  }}

  /* ---- Tablet ---- */
  @media (max-width: 900px) {{
    .fig-pair {{ grid-template-columns: 1fr; gap: 12px; }}
  }}

  /* ---- Phone ---- */
  @media (max-width: 640px) {{
    :root {{ --nav-h: 44px; }}
    html {{ -webkit-text-size-adjust: 100%; }}
    body {{ font-size: 14px; line-height: 1.55; }}
    header.db-h {{ padding: 18px 16px 14px; }}
    section.section {{ padding: 18px 16px 26px;
                        scroll-margin-top: calc(var(--nav-h) + 4px); }}
    .sec-h {{ margin: 0 -16px 16px; padding: 11px 16px;
              top: calc(var(--nav-h) - 1px);
              gap: 10px; }}
    nav.top {{ padding: 0 12px; gap: 8px; }}
    .brand {{ font-size: 11.5px; }}
    .navlinks a {{ padding: 12px 10px; font-size: 11px; }}
    .navlinks .num {{ width: 15px; height: 15px; font-size: 8.5px; }}
    h1 {{ font-size: 20px; line-height: 1.25; }}
    h3 {{ font-size: 14px; margin: 22px 0 10px; }}
    p, .lead {{ font-size: 13.5px; line-height: 1.6; }}

    .grid-2 {{ grid-template-columns: 1fr; }}

    /* Inline SVGs: keep their native readability — let the user scroll
       horizontally inside a container if the SVG is wider than the viewport.
       Otherwise scaling to 100% makes 10-px text into 4-px text on a 375-px
       phone, which is unreadable. */
    .fig-wrap {{ overflow-x: auto; -webkit-overflow-scrolling: touch;
                  margin: 12px -16px; padding: 0 16px; max-width: none; }}
    svg.figure-svg, svg.stage1-svg {{
      max-width: none; min-width: 720px; }}
    .fig-cap {{ font-size: 11px; padding: 0 4px; max-width: 720px; }}

    /* Trial table: horizontal scroll, keep first two columns sticky for
       orientation. */
    .trial-table-wrap {{ padding: 10px; }}
    .filters {{ gap: 6px; }}
    .filter-search {{ flex-basis: 100%; }}
    .filter-select, .filter-toggle, .filter-clear {{ font-size: 11px; }}
    .tt-scroll {{ max-height: 600px; }}
    table.trial-table {{ font-size: 11px; }}
    table.trial-table th, table.trial-table td {{ padding: 6px 8px; }}
    table.trial-table th:first-child,
    table.trial-table td:first-child {{
      position: sticky; left: 0; z-index: 1;
      background: var(--bg); box-shadow: 1px 0 0 var(--border); }}
    table.trial-table thead th:first-child {{ z-index: 3; background: var(--bg2); }}

    /* Tap targets and scroll hints */
    button, .filter-clear, a.cite, .pill {{ min-height: 24px; }}
    .scroll-hint {{ display: block; font-size: 10px; color: var(--text3);
                     padding: 4px 0 2px; text-align: right;
                     font-family: "SF Mono", monospace; }}

    /* Disable the back-to-top button on mobile (vertical real estate is precious) */
    #back-top {{ width: 36px; height: 36px; right: 14px; bottom: 14px; }}

    /* Sticky inside section can glitch on iOS Safari at narrow widths —
       fall back to non-sticky section header on phone */
    .sec-h {{ position: static; }}
  }}

  /* ---- Very narrow phone ---- */
  @media (max-width: 380px) {{
    section.section {{ padding-left: 12px; padding-right: 12px; }}
    .sec-h {{ margin: 0 -12px 14px; padding: 10px 12px; }}
    h1 {{ font-size: 18px; }}
  }}
</style>
</head>
<body>

<nav class="top">
  <div class="brand">Target Intelligence · <span>{TARGET_GENE}</span></div>
  <div class="navlinks">
    <a href="#genomic-landscape"><span class="num">1</span>Genomic landscape</a>
    <a href="#mechanism"><span class="num">2</span>Mechanism</a>
    <a href="#clinical"><span class="num">3</span>Clinical landscape</a>
    <a href="#synthesis"><span class="num">4</span>Synthesis</a>
    <a href="#references"><span class="num ref-num">R</span>References</a>
  </div>
</nav>

<header class="db-h">
  <div class="tpill">target intelligence</div>
  <h1>{title}</h1>
  <div class="meta">Generated {gen_date} · {length(REF_KEYS)} citations · {nrow(audit)} audit rows</div>
</header>

<div id="scroll-progress" aria-hidden="true"></div>

{section1}
{section2}
{section3}
{section4}
{section_refs}

<button id="back-top" type="button" aria-label="Back to top" title="Back to top">↑</button>
<div id="tt" class="tooltip" role="tooltip" aria-hidden="true"></div>

<script>
(function() {{
  // ---- Scroll progress bar ----
  const progressEl = document.getElementById("scroll-progress");
  function updateProgress() {{
    const h = document.documentElement;
    const max = h.scrollHeight - h.clientHeight;
    const pct = max > 0 ? (h.scrollTop / max) * 100 : 0;
    progressEl.style.width = pct + "%";
  }}
  window.addEventListener("scroll", updateProgress, {{ passive: true }});
  updateProgress();

  // ---- Back-to-top button ----
  const btEl = document.getElementById("back-top");
  function updateBackTop() {{
    if (window.scrollY > 400) btEl.classList.add("visible");
    else                       btEl.classList.remove("visible");
  }}
  window.addEventListener("scroll", updateBackTop, {{ passive: true }});
  btEl.addEventListener("click", function() {{
    window.scrollTo({{ top: 0, behavior: "smooth" }});
  }});
  updateBackTop();

  // ---- Active section in sticky nav (Intersection Observer) ----
  const navLinks = document.querySelectorAll("nav.top .navlinks a");
  const sectionMap = new Map();
  navLinks.forEach(function(a) {{
    const id = a.getAttribute("href").replace("#","");
    sectionMap.set(id, a);
  }});
  const sections = document.querySelectorAll("section.section");
  let lastActiveId = null;
  function setActive(id) {{
    if (id === lastActiveId) return;
    navLinks.forEach(function(a) {{ a.classList.remove("active"); }});
    const link = sectionMap.get(id);
    if (link) link.classList.add("active");
    lastActiveId = id;
  }}
  const navIO = new IntersectionObserver(function(entries) {{
    // Pick the entry closest to the top of the viewport that is intersecting
    let topMost = null;
    entries.forEach(function(e) {{
      if (e.isIntersecting) {{
        if (!topMost || e.boundingClientRect.top < topMost.boundingClientRect.top) {{
          topMost = e;
        }}
      }}
    }});
    if (topMost) setActive(topMost.target.id);
  }}, {{ rootMargin: "-30% 0px -65% 0px", threshold: 0 }});
  sections.forEach(function(s) {{ navIO.observe(s); }});

  // ---- Scroll-triggered fade-in + plot animations ----
  const animatables = document.querySelectorAll("section.section, .fig-wrap, img.fig, svg.figure-svg, svg.stage1-svg");
  const io = new IntersectionObserver(function(entries) {{
    entries.forEach(function(e) {{
      if (e.isIntersecting) {{
        e.target.classList.add("visible");
        const t = e.target;
        // Bar/segment grow-in across all SVGs
        if (t.querySelectorAll) {{
          t.querySelectorAll(".bar, .f3-seg, .f6-bar").forEach(function(b) {{
            const w = b.getAttribute("data-target-w");
            if (w !== null) b.setAttribute("width", w);
            const x = b.getAttribute("data-target-x");
            if (x !== null) b.setAttribute("x", x);
          }});
          t.querySelectorAll(".row-pct").forEach(function(tx) {{
            const xv = tx.getAttribute("data-target-x");
            if (xv !== null) tx.setAttribute("x", xv);
          }});
        }}
        io.unobserve(e.target);
      }}
    }});
  }}, {{ threshold: 0.08, rootMargin: "0px 0px -40px 0px" }});
  animatables.forEach(function(el) {{ io.observe(el); }});

  // ---- Tooltip ----
  const tt = document.getElementById("tt");
  function showTip(text, evt) {{
    tt.innerHTML = text;
    tt.classList.add("visible");
    tt.setAttribute("aria-hidden", "false");
    moveTip(evt);
  }}
  function hideTip() {{
    tt.classList.remove("visible");
    tt.setAttribute("aria-hidden", "true");
  }}
  function moveTip(evt) {{
    const pad = 14;
    let x = evt.clientX + pad;
    let y = evt.clientY + pad;
    const w = tt.offsetWidth, h = tt.offsetHeight;
    if (x + w > window.innerWidth - 8)  x = evt.clientX - w - pad;
    if (y + h > window.innerHeight - 8) y = evt.clientY - h - pad;
    tt.style.left = x + "px";
    tt.style.top  = y + "px";
  }}
  function wire(selector, builder) {{
    document.querySelectorAll(selector).forEach(function(g) {{
      g.addEventListener("mouseenter", function(evt) {{
        showTip(builder(g), evt);
      }});
      g.addEventListener("mousemove", moveTip);
      g.addEventListener("mouseleave", hideTip);
      // Touch support: tap shows the tooltip; tap elsewhere hides it
      g.addEventListener("touchstart", function(evt) {{
        if (evt.touches && evt.touches[0]) {{
          const t = evt.touches[0];
          showTip(builder(g), {{ clientX: t.clientX, clientY: t.clientY }});
          evt.stopPropagation();
        }}
      }}, {{ passive: true }});
    }});
  }}
  // Tap anywhere outside a wired element to dismiss the tooltip
  document.addEventListener("touchstart", function(evt) {{
    const inside = evt.target.closest(".bar-row, .f2-panel, .f3-row, .f4-cell-g, .f5-row, .f6-row");
    if (!inside) hideTip();
  }}, {{ passive: true }});
  function row(label, value) {{
    return "<div class=\\"tt-row\\"><span>" + label + "</span><b>" + value + "</b></div>";
  }}
  // Figure 1 — Stage 1 bars
  wire("svg.stage1-svg .bar-row", function(g) {{
    return "<div class=\\"tt-title\\">" + g.dataset.cohort + "</div>" +
           row("Homozygous (CN=0)", g.dataset.homdel + "%") +
           row("Heterozygous (CN=1)", g.dataset.hetdel + "%") +
           row("Total deletion", parseFloat(g.dataset.total).toFixed(1) + "%") +
           "<div class=\\"tt-sub\\">n = " + g.dataset.n + " samples (ABSOLUTE QC pass, primary tumour)</div>";
  }});
  // Figure 2 — CN→expression panels
  wire(".fig2-svg .f2-panel", function(g) {{
    const d = g.dataset;
    return "<div class=\\"tt-title\\">" + d.cohort + " · ρ = " + d.rho + "</div>" +
           row("CN=0 median",   d.cn0m) +
           row("CN=1 median",   d.cn1m) +
           row("CN=2 median",   d.cn2m) +
           row("CN=3+ median",  d.cn3m) +
           "<div class=\\"tt-sub\\">n per bin: " + d.cn0n + " / " + d.cn1n + " / " + d.cn2n + " / " + d.cn3n +
           " (total " + d.n + ")</div>";
  }});
  // Figure 3 — focality rows
  wire(".fig3-svg .f3-row", function(g) {{
    const d = g.dataset;
    return "<div class=\\"tt-title\\">" + d.cohort + " · " + d["class"] + "</div>" +
           row("Focal (< 3 Mb)",          parseFloat(d.focal).toFixed(1) + "%") +
           row("Intermediate (3–25 Mb)",  parseFloat(d.inter).toFixed(1) + "%") +
           row("Arm-level (≥ 25 Mb)",     parseFloat(d.arm).toFixed(1)   + "%") +
           row("Median footprint",        parseFloat(d.med).toFixed(2) + " Mb") +
           "<div class=\\"tt-sub\\">n = " + d.n + " deleted samples</div>";
  }});
  // Figure 4 — heatmap cells
  wire(".fig4-svg .f4-cell-g", function(g) {{
    const d = g.dataset;
    const sign = parseFloat(d.enrich) >= 0 ? "+" : "";
    return "<div class=\\"tt-title\\">" + d.cohort + " · " + d.gene + "</div>" +
           row("Anyloss in MTAP-homdel", parseFloat(d.anyloss).toFixed(1) + "%") +
           row("Homdel in MTAP-homdel",  parseFloat(d.homdel).toFixed(1)  + "%") +
           row("Anyloss in MTAP-intact", parseFloat(d.baseline).toFixed(1) + "%") +
           row("Enrichment (Δ pp)",      sign + parseFloat(d.enrich).toFixed(1)) +
           "<div class=\\"tt-sub\\">n MTAP-homdel patients = " + d.nmtap + "</div>";
  }});
  // Figure 5 — lollipop rows
  wire(".fig5-svg .f5-row", function(g) {{
    const d = g.dataset;
    const hr = parseFloat(d.homdelRate);
    const br = parseFloat(d.baselineRate);
    const enr = (br > 0) ? (hr / br).toFixed(2) + "×" : "—";
    return "<div class=\\"tt-title\\">" + d.cohort + " · " + d.gene + "</div>" +
           row("MTAP-homdel rate",  hr.toFixed(1) + "%") +
           row("MTAP-intact rate",  br.toFixed(1) + "%") +
           row("Enrichment ratio",  enr) +
           "<div class=\\"tt-sub\\">n MTAP-homdel = " + d.nHomdel + " · cohort n = " + d.nCohort + "</div>";
  }});
  // ---- Filterable trial table ----
  const searchEl  = document.getElementById("tt-search");
  const classEl   = document.getElementById("tt-class");
  const phaseEl   = document.getElementById("tt-phase");
  const statusEl  = document.getElementById("tt-status");
  const bmEl      = document.getElementById("tt-bm");
  const hasResEl  = document.getElementById("tt-has-results");
  const clearEl   = document.getElementById("tt-clear");
  const visibleEl = document.getElementById("tt-visible");
  const trialRows = document.querySelectorAll("tr.tt-row");

  function applyFilters() {{
    const q   = (searchEl.value || "").trim().toLowerCase();
    const cl  = classEl.value;
    const ph  = phaseEl.value;
    const st  = statusEl.value;
    const bm  = bmEl.value;
    const hr  = hasResEl.checked;
    let shown = 0;
    trialRows.forEach(function(r) {{
      const d = r.dataset;
      const ok = (!q  || d.search.indexOf(q) !== -1) &&
                 (!cl || d["class"] === cl) &&
                 (!ph || d.phase  === ph) &&
                 (!st || d.status === st) &&
                 (!bm || d.biomarker === bm) &&
                 (!hr || d.hasResults === "yes");
      r.hidden = !ok;
      if (ok) shown++;
      // Hide detail row when its summary row is hidden
      const detail = r.nextElementSibling;
      if (detail && detail.classList.contains("tt-detail-row")) {{
        if (!ok) {{
          detail.hidden = true;
          r.classList.remove("tt-active");
          const btn = r.querySelector(".results-toggle");
          if (btn) btn.setAttribute("aria-expanded", "false");
        }}
      }}
    }});
    visibleEl.textContent = shown;
  }}
  [searchEl, classEl, phaseEl, statusEl, bmEl, hasResEl].forEach(function(el) {{
    if (!el) return;
    el.addEventListener("input",  applyFilters);
    el.addEventListener("change", applyFilters);
  }});
  if (clearEl) {{
    clearEl.addEventListener("click", function() {{
      searchEl.value = "";
      classEl.value = ""; phaseEl.value = ""; statusEl.value = ""; bmEl.value = "";
      hasResEl.checked = false;
      applyFilters();
    }});
  }}

  // Row click to expand detail
  trialRows.forEach(function(r) {{
    r.addEventListener("click", function(e) {{
      if (e.target.closest("a")) return;  // skip NCT link
      const detail = r.nextElementSibling;
      if (!detail || !detail.classList.contains("tt-detail-row")) return;
      const isOpen = !detail.hidden;
      detail.hidden = isOpen;
      r.classList.toggle("tt-active", !isOpen);
      const btn = r.querySelector(".results-toggle");
      if (btn) btn.setAttribute("aria-expanded", isOpen ? "false" : "true");
    }});
  }});

  // ---- Sortable column headers ----
  const ttbody = document.querySelector("table.trial-table tbody");
  if (ttbody) {{
    const sortHeaders = document.querySelectorAll("table.trial-table th.tt-sort");
    let lastKey = null, lastDir = 1;
    function clearSortIndicators() {{
      sortHeaders.forEach(function(h) {{
        h.classList.remove("tt-sort-asc","tt-sort-desc");
      }});
    }}
    function sortVal(row, key, type) {{
      const k = "sort" + key.charAt(0).toUpperCase() + key.slice(1);
      let v = row.dataset[k];
      if (v === undefined || v === null || v === "") return type === "num" ? -Infinity : "";
      if (type === "num") {{
        const n = parseFloat(v);
        return isNaN(n) ? -Infinity : n;
      }}
      return v.toLowerCase();
    }}
    function doSort(key, type, dir) {{
      lastKey = key; lastDir = dir;
      clearSortIndicators();
      const h = document.querySelector("table.trial-table th.tt-sort[data-key=\\"" + key + "\\"]");
      if (h) h.classList.add(dir === 1 ? "tt-sort-asc" : "tt-sort-desc");
      const pairs = [];
      trialRows.forEach(function(r) {{
        const det = r.nextElementSibling;
        pairs.push([r, (det && det.classList.contains("tt-detail-row")) ? det : null]);
      }});
      pairs.sort(function(a, b) {{
        const va = sortVal(a[0], key, type);
        const vb = sortVal(b[0], key, type);
        if (va < vb) return -1 * dir;
        if (va > vb) return  1 * dir;
        return 0;
      }});
      const frag = document.createDocumentFragment();
      pairs.forEach(function(p) {{
        frag.appendChild(p[0]);
        if (p[1]) frag.appendChild(p[1]);
      }});
      ttbody.appendChild(frag);
    }}
    sortHeaders.forEach(function(h) {{
      h.addEventListener("click", function() {{
        const key  = h.dataset.key;
        const type = h.dataset.type || "text";
        const dir  = (lastKey === key) ? -lastDir : 1;
        doSort(key, type, dir);
      }});
    }});
    // Default sort on load: ORR descending (highest ORRs at the top)
    doSort("orr", "num", -1);
  }}
}})();
</script>

</body>
</html>
', .open = "{", .close = "}")

out <- fs::path(RESULTS_DIR, "target_intel_dashboard.html")
writeLines(html, out)
message(glue::glue("[done] dashboard written: {out}"))
message(glue::glue("[done] file size: {format(file_info(out)$size, big.mark=',')} bytes"))
