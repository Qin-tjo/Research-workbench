## Panel 3 analysis — clinical landscape across MTAP-axis drugs.
## Reads:  results/06_clinical_trials.parquet (from Stage 6 fetcher)
## Writes:
##   results/06_clinical_landscape.png  — Gantt-style timeline
##   results/06_clinical_summary.parquet — per-drug rollup

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(glue)
  library(fs)
  library(yaml)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))
source(file.path(SCRIPT_DIR, "utils/style.R"))

DATA_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"

trials <- as.data.table(read_parquet(fs::path(RESULTS_DIR, "06_clinical_trials.parquet")))

# ---------------------------------------------------------------------------
# Biomarker-selection classifier (from eligibility text + keywords + title)
# ---------------------------------------------------------------------------
classify_biomarker <- function(elig, kws, title, cond) {
  txt <- tolower(paste(elig, kws, title, cond, sep = " || "))
  has_mtap   <- grepl("mtap", txt) &&
                grepl("(homo[a-z]*\\s*del|null|deficien|loss|hom-?del|-/-)", txt)
  has_cdkn2a <- grepl("cdkn2a", txt) &&
                grepl("(homo[a-z]*\\s*del|null|deficien|loss|hom-?del|-/-)", txt)
  if (has_mtap && has_cdkn2a)        return("MTAP + CDKN2A homdel required")
  if (has_mtap)                      return("MTAP homdel required")
  if (has_cdkn2a)                    return("CDKN2A homdel required")
  if (grepl("mtap", txt))            return("MTAP status reported (not required)")
  if (grepl("9p21", txt))            return("9p21 loss referenced")
  "Not biomarker-selected"
}
trials[, biomarker_strategy := mapply(classify_biomarker,
                                       eligibility_excerpt, keywords,
                                       brief_title, conditions)]

# ---------------------------------------------------------------------------
# Combination-strategy classifier (from interventions list)
# ---------------------------------------------------------------------------
classify_combo <- function(interventions, drug_of_interest) {
  if (is.na(interventions) || !nzchar(interventions)) return("Unknown")
  parts <- trimws(strsplit(interventions, "\\|")[[1]])
  others <- parts[!grepl(drug_of_interest, parts, ignore.case = TRUE) &
                  !grepl(tolower(drug_of_interest), tolower(parts), fixed = TRUE)]
  # Filter placebo / standard-of-care
  others <- others[!grepl("placebo|standard of care|investigator", others, ignore.case = TRUE)]
  if (length(others) == 0) return("Monotherapy")

  txt <- tolower(paste(others, collapse = " | "))
  tags <- c()
  if (grepl("pembrolizumab|nivolumab|atezolizumab|durvalumab|ipilimumab|cemiplimab|tislelizumab|immune checkpoint|anti-pd-?[l1]?", txt))
    tags <- c(tags, "IO")
  if (grepl("osimertinib|erlotinib|gefitinib|afatinib|dacomitinib|amivantamab|lazertinib|egfr", txt))
    tags <- c(tags, "EGFR")
  if (grepl("sotorasib|adagrasib|divarasib|kras|rmc-6236|rmc-9805|ras\\(on\\)|ras-on", txt))
    tags <- c(tags, "KRAS/RAS")
  if (grepl("olaparib|niraparib|talazoparib|rucaparib|parp", txt))
    tags <- c(tags, "PARP")
  if (grepl("trastuzumab|tucatinib|her2|t-dxd|t-dm1|trastuzumab deruxtecan", txt))
    tags <- c(tags, "HER2")
  if (grepl("pemetrexed|gemcitabine|cisplatin|carboplatin|paclitaxel|docetaxel|folfir|folfox|nab-?paclitaxel|chemo", txt))
    tags <- c(tags, "chemo")
  if (grepl("sacituzumab|trodelvy|trastuzumab deruxtecan|antibody[ -]?drug conjugate", txt))
    tags <- c(tags, "ADC")
  if (grepl("alectinib|crizotinib|brigatinib|lorlatinib|alk", txt))
    tags <- c(tags, "ALK")
  if (grepl("encorafenib|dabrafenib|trametinib|mek|braf", txt))
    tags <- c(tags, "MAPK")
  if (length(tags) == 0) tags <- "Other targeted"

  paste0("Combo: ", paste(unique(tags), collapse = " + "))
}
trials[, combination_strategy := mapply(classify_combo,
                                         interventions_all,
                                         canonical_drug)]

# Short indication tag from conditions
shorten_cond <- function(cond) {
  if (is.na(cond) || !nzchar(cond)) return("")
  cond_l <- tolower(cond)
  tags <- c()
  if (grepl("non.?small.?cell lung|nsclc",      cond_l)) tags <- c(tags, "NSCLC")
  if (grepl("small.?cell lung|sclc",            cond_l)) tags <- c(tags, "SCLC")
  if (grepl("pancrea",                          cond_l)) tags <- c(tags, "PDAC")
  if (grepl("biliary|cholang",                  cond_l)) tags <- c(tags, "Biliary")
  if (grepl("urothel|bladder",                  cond_l)) tags <- c(tags, "Urothelial")
  if (grepl("mesothel",                         cond_l)) tags <- c(tags, "Mesothelioma")
  if (grepl("glioblast|gbm|glioma",             cond_l)) tags <- c(tags, "Glioma/GBM")
  if (grepl("head and neck|hnscc",              cond_l)) tags <- c(tags, "HNSCC")
  if (grepl("esophag|gastric|stomach|gastroesoph", cond_l)) tags <- c(tags, "GI-upper")
  if (grepl("colorect|colon|rectal",            cond_l)) tags <- c(tags, "CRC")
  if (grepl("breast",                           cond_l)) tags <- c(tags, "Breast")
  if (grepl("melanom",                          cond_l)) tags <- c(tags, "Melanoma")
  if (grepl("ovarian|fallopian|peritoneal",     cond_l)) tags <- c(tags, "Ovarian")
  if (grepl("sarcoma",                          cond_l)) tags <- c(tags, "Sarcoma")
  if (grepl("solid tumor|solid tumour|advanced solid", cond_l)) tags <- c(tags, "Solid (basket)")
  if (length(tags) == 0) tags <- "(see full)"
  paste(unique(tags), collapse = " · ")
}
trials[, indications_short := vapply(conditions, shorten_cond, character(1))]


# Parse dates (CT.gov returns YYYY-MM-DD or sometimes YYYY-MM)
parse_ct_date <- function(x) {
  x <- as.character(x)
  ymd <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  ym  <- suppressWarnings(as.Date(paste0(x, "-15"), format = "%Y-%m-%d"))
  fifelse(is.na(ymd), ym, ymd)
}
trials[, start_d         := parse_ct_date(start_date)]
trials[, end_d           := parse_ct_date(completion_date)]
trials[, primary_end_d   := parse_ct_date(primary_completion)]
trials[, last_update_d   := parse_ct_date(last_update_date)]

# Normalise status into a 4-level vocabulary
status_map <- c(
  RECRUITING              = "Active",
  ACTIVE_NOT_RECRUITING   = "Active",
  ENROLLING_BY_INVITATION = "Active",
  NOT_YET_RECRUITING      = "Planned",
  COMPLETED               = "Completed",
  TERMINATED              = "Terminated",
  WITHDRAWN               = "Terminated",
  SUSPENDED               = "Terminated"
)
trials[, status_short := factor(unname(status_map[overall_status]),
                                levels = c("Active","Planned","Completed","Terminated"))]

# Normalise phase
trials[, phase_short := fcase(
  phase %in% c("EARLY_PHASE1", "Ph1"), "Ph1",
  phase == "Ph1/Ph2",                  "Ph1/2",
  phase == "Ph2",                      "Ph2",
  phase == "Ph2/Ph3",                  "Ph2/3",
  phase == "Ph3",                      "Ph3",
  default = phase
)]
trials[, phase_short := factor(phase_short, levels = c("Ph1","Ph1/2","Ph2","Ph2/3","Ph3"))]

# Today (clamp future planned starts and ongoing-no-end to today's bar end)
TODAY <- as.Date("2026-06-16")
trials[, x_start := start_d]
trials[, x_end   := fcase(
  !is.na(end_d),         end_d,
  !is.na(primary_end_d), primary_end_d,
  status_short == "Active",    TODAY,
  status_short == "Planned",   pmax(start_d, TODAY) + 365,   # 1y projection
  default = pmax(start_d, last_update_d, na.rm = TRUE)
)]

# Drop rows with no usable start
trials <- trials[!is.na(x_start)]

# Mechanism-class order (most-advanced field first)
class_levels <- c("MTA-cooperative PRMT5i",
                  "MAT2A inhibitor",
                  "First-gen PRMT5i (SAM-competitive)")
trials[, mechanism_class := factor(mechanism_class, levels = class_levels)]

# Drug order within class — by max phase reached, then earliest start
drug_order <- trials[, .(
  max_phase = max(as.integer(phase_short), na.rm = TRUE),
  earliest  = min(x_start, na.rm = TRUE),
  n_active  = sum(status_short == "Active")
), by = .(mechanism_class, canonical_drug)
][order(mechanism_class, -max_phase, earliest)]

trials[, drug_f := factor(canonical_drug, levels = rev(drug_order$canonical_drug))]

# Per-trial y position: drug first, then rank within drug by x_start (earliest at top)
setorder(trials, mechanism_class, drug_f, x_start)
trials[, y_row := paste0(canonical_drug, "  ", nct_id)]

# Build a per-row factor sorted by class then drug then start
row_levels <- trials[, .(y_row, ord = .I)][order(-ord)]$y_row
trials[, y_row := factor(y_row, levels = row_levels)]

# ---------------------------------------------------------------------------
# Plot — Gantt timeline
# ---------------------------------------------------------------------------
# Class separators (horizontal grey lines)
class_breaks <- cumsum(rev(table(trials$mechanism_class)))
sep_y <- nrow(trials) - class_breaks[-length(class_breaks)] + 0.5

phase_palette <- c(
  Ph1   = "#9FE1CB",
  `Ph1/2` = "#5DCAA5",
  Ph2   = "#1D9E75",
  `Ph2/3` = "#0F6E56",
  Ph3   = "#04342C"
)
status_shape <- c(Active = 16, Planned = 1, Completed = 18, Terminated = 4)

p <- ggplot(trials) +
  geom_hline(yintercept = sep_y, color = "grey75",
             linewidth = 0.5, linetype = "dashed") +
  geom_segment(aes(x = x_start, xend = x_end,
                   y = y_row, yend = y_row,
                   color = phase_short),
               linewidth = 2.6, lineend = "round") +
  geom_point(aes(x = x_start, y = y_row, shape = status_short),
             size = 1.9, color = "#1a1a18", stroke = 0.4) +
  geom_text(aes(x = x_end + 60, y = y_row,
                label = sprintf("%s · %s", nct_id, status_short)),
            hjust = 0, size = 2.4, family = "mono", color = "#444") +
  scale_color_manual(values = phase_palette, name = "Phase", drop = FALSE) +
  scale_shape_manual(values = status_shape, name = "Status at start", drop = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               limits = as.Date(c("2016-01-01", "2029-06-01")),
               expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title    = "Clinical landscape of MTAP-axis drugs",
    subtitle = wrap_text("Each row is a ClinicalTrials.gov entry. Bar spans trial start → completion (active trials clamped to today; planned trials projected +1y). Bar colour = phase; start-of-bar marker = status. Drugs grouped by mechanism class (dashed separators); within class, ordered by max phase reached then earliest start.",
                          width = 150),
    x = NULL, y = NULL,
    caption = wrap_text(glue("Source: ClinicalTrials.gov v2 (fetched {as.character(TODAY)}). {nrow(trials)} unique trials across {uniqueN(trials$canonical_drug)} drugs in {length(class_levels)} mechanism classes."),
                          width = 165)
  ) +
  theme_target_intel(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y        = element_text(family = "mono", size = 8),
    legend.position    = "top",
    legend.box         = "horizontal"
  )

out_png <- fs::path(RESULTS_DIR, "06_clinical_landscape.png")
ggsave(out_png, p,
       width  = 16,
       height = 0.30 * nrow(trials) + 3.5, dpi = 160)
message(glue("[plot] {out_png}"))

# ---------------------------------------------------------------------------
# Per-drug rollup
# ---------------------------------------------------------------------------
rollup <- trials[, .(
  n_trials        = .N,
  n_active        = sum(status_short == "Active"),
  n_planned       = sum(status_short == "Planned"),
  n_completed     = sum(status_short == "Completed"),
  n_terminated    = sum(status_short == "Terminated"),
  max_phase       = as.character(levels(phase_short)[max(as.integer(phase_short), na.rm = TRUE)]),
  earliest_start  = as.character(min(x_start, na.rm = TRUE)),
  latest_update   = as.character(max(last_update_d, na.rm = TRUE))
), by = .(mechanism_class, canonical_drug)
][order(mechanism_class, -n_active, -n_trials)]

out_pq <- fs::path(RESULTS_DIR, "06_clinical_summary.parquet")
write_parquet(rollup, out_pq)
print(rollup)

# Persist the enriched per-trial table so the renderer doesn't need to
# re-classify on every invocation.
enriched_pq <- fs::path(RESULTS_DIR, "06_clinical_trials_enriched.parquet")
write_parquet(trials, enriched_pq)

# Audit
write_audit(
  audit_row(
    analysis    = "06_panel3_clinical",
    source      = "ClinicalTrials.gov v2 (Stage 6 fetch)",
    n_in        = nrow(trials),
    n_excluded  = 0L,
    test        = "descriptive Gantt timeline",
    adjust      = "none",
    output_path = paste(out_png, out_pq, sep = "; "),
    notes       = glue("classes={length(class_levels)}; drugs={uniqueN(trials$canonical_drug)}; active={sum(trials$status_short=='Active')}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Panel 3 complete.")
