## Panel 3 fetcher — pull ClinicalTrials.gov v2 entries for every drug in
## data/panel3_drugs.yaml.  One JSON cache per (drug, search_term).
## No auth; the API is fully public.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fs)
  library(glue)
  library(jsonlite)
  library(httr)
  library(yaml)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))

DATA_DIR   <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"
CT_DIR     <- fs::path(CACHE_DIR, "ctgov")
dir_create(CT_DIR)

drugs_cfg <- yaml::read_yaml(fs::path(DATA_DIR, "panel3_drugs.yaml"))$drugs

CTGOV_BASE <- "https://clinicaltrials.gov/api/v2/studies"

fetch_term <- function(term) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", term)
  dest <- fs::path(CT_DIR, paste0(safe, ".json"))
  if (file_exists(dest) && file_info(dest)$size > 50) {
    return(invisible(dest))
  }
  message(glue("[fetch] {term}"))
  q <- list(`query.intr` = term, pageSize = 100, format = "json")
  resp <- httr::GET(CTGOV_BASE, query = q,
                    httr::accept_json(), httr::timeout(120))
  if (httr::http_error(resp)) {
    message(glue("  failed: {httr::http_status(resp)$message}"))
    return(invisible(NULL))
  }
  writeLines(httr::content(resp, "text", encoding = "UTF-8"), dest)
  invisible(dest)
}

parse_study <- function(s, canonical_drug, mech_class, sponsor_canonical) {
  p <- s$protocolSection
  if (is.null(p)) return(NULL)
  ident <- p$identificationModule
  status <- p$statusModule
  cond   <- p$conditionsModule
  spons  <- p$sponsorCollaboratorsModule
  design <- p$designModule
  oc     <- p$outcomesModule
  arms   <- p$armsInterventionsModule
  elig   <- p$eligibilityModule
  desc   <- p$descriptionModule

  loc_summary <- {
    locs <- p$contactsLocationsModule$locations
    if (is.null(locs) || length(locs) == 0) ""
    else {
      countries <- unique(vapply(locs, function(l) l$country %||% NA_character_, character(1)))
      sprintf("%d sites / %d countries", length(locs), length(stats::na.omit(countries)))
    }
  }

  phases <- design$phases
  phase_str <- if (is.null(phases) || length(phases) == 0) "NA"
                else paste(gsub("^PHASE", "Ph", phases), collapse = "/")

  # All interventions (name + type + otherNames)
  iv_list <- arms$interventions
  iv_names <- if (is.null(iv_list)) character(0) else
    vapply(iv_list, function(x) x$name %||% NA_character_, character(1))
  iv_types <- if (is.null(iv_list)) character(0) else
    vapply(iv_list, function(x) x$type %||% NA_character_, character(1))
  interventions_all <- paste(iv_names, collapse = " | ")
  interventions_types <- paste(unique(iv_types), collapse = ", ")

  # Primary endpoint(s)
  primary_eps <- if (is.null(oc$primaryOutcomes)) "" else
    paste(vapply(oc$primaryOutcomes, function(o) o$measure %||% "", character(1)),
          collapse = " ; ")

  # Eligibility text (truncate; we only need biomarker / inclusion clauses)
  elig_text <- elig$eligibilityCriteria %||% ""
  elig_excerpt <- substr(elig_text, 1, 2500)

  brief_summary <- desc$briefSummary %||% ""
  brief_summary <- substr(brief_summary, 1, 1500)

  data.table(
    nct_id              = ident$nctId %||% NA_character_,
    canonical_drug      = canonical_drug,
    mechanism_class     = mech_class,
    canonical_sponsor   = sponsor_canonical,
    brief_title         = ident$briefTitle %||% NA_character_,
    brief_summary       = brief_summary,
    phase               = phase_str,
    overall_status      = status$overallStatus %||% NA_character_,
    start_date          = status$startDateStruct$date %||% NA_character_,
    primary_completion  = status$primaryCompletionDateStruct$date %||% NA_character_,
    completion_date     = status$completionDateStruct$date %||% NA_character_,
    last_update_date    = status$lastUpdatePostDateStruct$date %||% NA_character_,
    has_results         = isTRUE(status$resultsFirstPostDateStruct$type == "ACTUAL"),
    conditions          = paste(cond$conditions, collapse = "; "),
    keywords            = paste(cond$keywords, collapse = "; "),
    lead_sponsor        = spons$leadSponsor$name %||% NA_character_,
    study_type          = design$studyType %||% NA_character_,
    enrollment_count    = design$enrollmentInfo$count %||% NA_integer_,
    allocation          = design$designInfo$allocation %||% NA_character_,
    interventional_model= design$designInfo$interventionalModel %||% NA_character_,
    interventions_all   = interventions_all,
    interventions_types = interventions_types,
    primary_endpoints   = primary_eps,
    eligibility_excerpt = elig_excerpt,
    locations_summary   = loc_summary
  )
}

# ---------------------------------------------------------------------------
# Fetch every search term + parse
# ---------------------------------------------------------------------------
all_rows <- list()
for (d in drugs_cfg) {
  rows <- list()
  for (term in d$search_terms) {
    dest <- fetch_term(term)
    if (is.null(dest) || !file_exists(dest)) next
    j <- jsonlite::fromJSON(dest, simplifyVector = FALSE)
    studies <- j$studies
    if (is.null(studies) || length(studies) == 0) next
    for (s in studies) {
      r <- parse_study(s, canonical_drug = d$name,
                       mech_class = d$mechanism_class,
                       sponsor_canonical = d$sponsor)
      if (!is.null(r)) rows[[length(rows) + 1]] <- r
    }
  }
  if (length(rows)) {
    dt <- rbindlist(rows, fill = TRUE)
    # Dedup by NCT within drug — same study returned by multiple search terms
    dt <- unique(dt, by = "nct_id")
    all_rows[[d$name]] <- dt
    message(glue("[drug] {d$name}: {nrow(dt)} unique trials"))
  } else {
    message(glue("[drug] {d$name}: no trials found"))
  }
}

trials <- rbindlist(all_rows, fill = TRUE)
# Cross-drug dedup: same NCT, different drug -> keep all (combo trials)
out_pq <- fs::path(RESULTS_DIR, "06_clinical_trials.parquet")
write_parquet(trials, out_pq)
message(glue("[done] {nrow(trials)} trial rows across {uniqueN(trials$canonical_drug)} drugs; cached to {out_pq}"))

write_audit(
  audit_row(
    analysis    = "06_fetch_ctgov",
    source      = "ClinicalTrials.gov v2 REST API",
    n_in        = nrow(trials),
    n_excluded  = 0L,
    output_path = out_pq,
    notes       = glue("drugs={length(drugs_cfg)}; unique_NCT={uniqueN(trials$nct_id)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)
