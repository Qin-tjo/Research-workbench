## Panel 3 renderer — collapsible drug/trial cards with biomarker, combo,
## endpoint, eligibility excerpt and (when available) curated readouts.

suppressPackageStartupMessages({
  library(yaml)
  library(arrow)
  library(data.table)
  library(glue)
  library(fs)
  library(htmltools)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))

DATA_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"
cit      <- yaml::read_yaml(fs::path(DATA_DIR, "citations.yaml"))
drugs    <- yaml::read_yaml(fs::path(DATA_DIR, "panel3_drugs.yaml"))$drugs
results  <- yaml::read_yaml(fs::path(DATA_DIR, "panel3_results.yaml"))$readouts

trials   <- as.data.table(read_parquet(fs::path(RESULTS_DIR,
                            "06_clinical_trials_enriched.parquet")))
rollup   <- as.data.table(read_parquet(fs::path(RESULTS_DIR,
                            "06_clinical_summary.parquet")))

# Re-derive status_short / phase_short (same logic as Stage 9)
status_map <- c(
  RECRUITING = "Active", ACTIVE_NOT_RECRUITING = "Active",
  ENROLLING_BY_INVITATION = "Active", NOT_YET_RECRUITING = "Planned",
  COMPLETED = "Completed", TERMINATED = "Terminated",
  WITHDRAWN = "Terminated", SUSPENDED = "Terminated"
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

esc <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("")
  htmltools::htmlEscape(as.character(x))
}
nz  <- function(x) !is.null(x) && length(x) >= 1 && nzchar(trimws(as.character(x)[1]))

# Citation chip
cite_chip <- function(key) {
  c <- cit[[key]]
  if (is.null(c)) return(sprintf('<span class="cite missing">[%s?]</span>', esc(key)))
  short <- sub(",.*", "", c$authors)
  sprintf('<a class="cite" href="%s" target="_blank" rel="noopener" title="%s">%s %d</a>',
          c$url, esc(c$title), esc(short), c$year)
}

# Biomarker pill colour
bm_pill <- function(s) {
  if (grepl("MTAP \\+ CDKN2A", s))           return('<span class="pill bm-strict">MTAP + CDKN2A homdel</span>')
  if (grepl("MTAP homdel required", s))      return('<span class="pill bm-strict">MTAP homdel required</span>')
  if (grepl("CDKN2A homdel required", s))    return('<span class="pill bm-strict">CDKN2A homdel required</span>')
  if (grepl("status reported", s))           return('<span class="pill bm-soft">MTAP reported, not required</span>')
  if (grepl("9p21", s))                      return('<span class="pill bm-soft">9p21 loss referenced</span>')
  '<span class="pill bm-none">Not biomarker-selected</span>'
}

# Combo pill
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

# Trial card  -- collapsible via <details>
trial_card <- function(r) {
  nct <- r$nct_id
  readout <- results[[nct]]
  readout_html <- if (is.null(readout)) "" else {
    finds <- paste(sprintf('<li>%s</li>', vapply(readout$key_findings, esc, character(1))),
                    collapse = "")
    extras <- if (nz(readout$additional_citations))
      sprintf(' · also %s',
              paste(vapply(readout$additional_citations, cite_chip, character(1)),
                    collapse = " "))
    else ""
    sprintf('
      <div class="readout">
        <div class="readout-head">📊 %s · %s%s</div>
        <ul>%s</ul>
      </div>',
      esc(readout$headline), cite_chip(readout$citation), extras, finds)
  }

  elig_short <- esc(substr(r$eligibility_excerpt, 1, 700))
  elig_html <- if (nz(elig_short))
    sprintf('<details class="elig"><summary>Eligibility excerpt</summary><div class="elig-body">%s%s</div></details>',
            elig_short,
            ifelse(nchar(r$eligibility_excerpt) > 700, " <em>…(truncated)</em>", ""))
    else ""

  summary_html <- if (nz(r$brief_summary))
    sprintf('<div class="brief"><b>Summary:</b> %s</div>', esc(r$brief_summary))
    else ""

  endpoint_html <- if (nz(r$primary_endpoints))
    sprintf('<div class="endpoint"><b>Primary endpoint(s):</b> %s</div>',
            esc(r$primary_endpoints))
    else ""

  intervention_html <- if (nz(r$interventions_all))
    sprintf('<div class="interv"><b>Interventions:</b> %s</div>',
            esc(r$interventions_all))
    else ""

  sprintf('
  <details class="trial">
    <summary>
      <span class="trial-head">
        <a class="nct" href="https://clinicaltrials.gov/study/%s" target="_blank" rel="noopener">%s</a>
        <span class="pill %s">%s</span>
        <span class="pill %s">%s</span>
        %s
        %s
      </span>
      <span class="trial-title">%s</span>
      <span class="trial-meta">indications: %s · start %s · updated %s</span>
    </summary>
    <div class="trial-body">
      %s
      %s
      %s
      %s
      %s
    </div>
  </details>',
    esc(nct), esc(nct),
    paste0("ph-", gsub("/", "", tolower(as.character(r$phase_short)))),
    esc(r$phase_short),
    paste0("st-", tolower(as.character(r$status_short))),
    esc(r$status_short),
    bm_pill(r$biomarker_strategy),
    combo_pill(r$combination_strategy),
    esc(r$brief_title),
    esc(r$indications_short %||% ""),
    esc(r$start_date), esc(r$last_update_date),
    readout_html,
    summary_html,
    endpoint_html,
    intervention_html,
    elig_html
  )
}

# Drug card -- collapsible via <details>
drug_card <- function(d) {
  rows <- trials[canonical_drug == d$name]
  if (nrow(rows) == 0) {
    return(sprintf('<details class="drug"><summary><b>%s</b> <span class="empty">no CT.gov entries</span></summary></details>', esc(d$name)))
  }
  rows[, .phase_n := as.integer(phase_short)]
  setorder(rows, -.phase_n, status_short, start_date)
  rows[, .phase_n := NULL]
  rollup_row <- rollup[canonical_drug == d$name]

  summary_line <- sprintf(
    'max phase <b>%s</b> · <span class="st-active">%d active</span> · <span class="st-planned">%d planned</span> · <span class="st-completed">%d completed</span> · <span class="st-terminated">%d terminated</span>',
    esc(rollup_row$max_phase),
    rollup_row$n_active, rollup_row$n_planned,
    rollup_row$n_completed, rollup_row$n_terminated
  )

  pub <- d$primary_publication
  pub_chip <- if (nz(pub)) sprintf(' · primary publication %s', cite_chip(pub)) else ""

  aliases <- if (length(d$aliases))
    sprintf(' <span class="alias">aka %s</span>', esc(paste(d$aliases, collapse = ", ")))
    else ""

  notes_html <- if (!is.null(d$notes) && nz(d$notes))
    sprintf('<div class="notes">%s</div>', esc(trimws(d$notes)))
    else ""

  trial_blocks <- vapply(seq_len(nrow(rows)),
                         function(i) trial_card(rows[i]),
                         character(1))

  sprintf('
  <details class="drug" open>
    <summary>
      <span class="drug-name">%s</span>%s
      <span class="mech">%s · %s</span>
      <span class="rollup">%s%s</span>
    </summary>
    <div class="drug-body">
      %s
      %s
    </div>
  </details>',
    esc(d$name), aliases, esc(d$mechanism_class), esc(d$sponsor),
    summary_line, pub_chip,
    notes_html,
    paste(trial_blocks, collapse = "\n")
  )
}

classes <- unique(vapply(drugs, function(d) d$mechanism_class, character(1)))
class_blocks <- vapply(classes, function(cl) {
  idx <- which(vapply(drugs, function(d) d$mechanism_class == cl, logical(1)))
  sprintf('<section class="class"><h2>%s</h2>%s</section>',
          esc(cl), paste(vapply(idx, function(i) drug_card(drugs[[i]]), character(1)),
                         collapse = "\n"))
}, character(1))

html <- glue('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Panel 3 — Clinical landscape (MTAP-axis)</title>
<style>
  :root {{
    --teal-100:#9FE1CB; --teal-400:#1D9E75; --teal-600:#0F6E56; --teal-900:#04342C;
    --gray-50:#F1EFE8; --gray-100:#D3D1C7; --gray-400:#888780; --gray-900:#2C2C2A;
    --amber-100:#FAC775; --amber-600:#854F0B;
    --red-100:#F7C1C1; --red-800:#791F1F;
    --blue-100:#B5D4F4; --blue-600:#185FA5;
    --purple-100:#CECBF6; --purple-600:#534AB7;
    --bg:#ffffff; --bg2:#f7f6f3; --bg3:#f0efe9; --border:rgba(0,0,0,0.10);
    --text:#1a1a18; --text2:#5f5e5a; --text3:#888780;
  }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
          background: var(--bg); color: var(--text); font-size: 13px;
          line-height: 1.55; margin: 0; padding: 24px 30px 60px;
          max-width: 1240px; }}
  h1 {{ font-size: 22px; margin: 0 0 4px; }}
  .tpill {{ display: inline-block; background: #E1F5EE; color: var(--teal-900);
            font-size: 10px; font-weight: 600; padding: 3px 10px; border-radius: 20px;
            margin-bottom: 8px; letter-spacing: 0.03em; text-transform: uppercase; }}
  .lead {{ font-size: 12px; color: var(--text2); margin: 0 0 22px; max-width: 920px; }}
  h2 {{ font-size: 14px; font-weight: 700; color: var(--text);
        border-bottom: 0.5px solid var(--border); padding-bottom: 6px;
        margin: 30px 0 12px; letter-spacing: 0.01em; }}

  /* Drug card */
  details.drug {{ background: var(--bg2); border-radius: 10px; padding: 0;
                  border: 0.5px solid var(--border); margin-bottom: 12px;
                  overflow: hidden; }}
  details.drug > summary {{ padding: 12px 16px; cursor: pointer; list-style: none;
                            display: flex; flex-wrap: wrap; align-items: baseline;
                            gap: 8px; background: var(--bg2); }}
  details.drug > summary::-webkit-details-marker {{ display: none; }}
  details.drug > summary::before {{ content: "▶"; display: inline-block;
                                    transform: rotate(0deg); transition: transform 0.15s;
                                    font-size: 9px; color: var(--text3);
                                    margin-right: 6px; }}
  details.drug[open] > summary::before {{ transform: rotate(90deg); }}
  .drug-name {{ font-size: 14px; font-weight: 700; }}
  .alias    {{ font-size: 10px; color: var(--text3); font-weight: 500; }}
  .mech     {{ font-size: 10px; color: var(--text3); font-weight: 500;
               margin-left: auto; }}
  .rollup   {{ width: 100%; font-size: 11px; color: var(--text2);
               margin-top: 2px; }}
  .rollup .st-active     {{ color: var(--teal-600); }}
  .rollup .st-planned    {{ color: var(--amber-600); }}
  .rollup .st-completed  {{ color: var(--blue-600); }}
  .rollup .st-terminated {{ color: var(--red-800); }}
  .drug-body {{ padding: 8px 14px 14px; background: var(--bg); border-top: 0.5px solid var(--border); }}
  .notes {{ font-size: 11px; color: var(--text2); font-style: italic;
            margin-bottom: 10px; padding: 6px 10px; background: var(--bg2);
            border-radius: 5px; }}

  /* Trial card */
  details.trial {{ background: var(--bg); border: 0.5px solid var(--border);
                   border-radius: 7px; margin-bottom: 8px; }}
  details.trial > summary {{ padding: 8px 12px; cursor: pointer; list-style: none;
                             display: grid; gap: 4px; }}
  details.trial > summary::-webkit-details-marker {{ display: none; }}
  details.trial > summary::before {{ content: "▸"; color: var(--text3);
                                     font-size: 9px; margin-right: 4px; }}
  details.trial[open] > summary::before {{ content: "▾"; }}
  details.trial[open] > summary {{ border-bottom: 0.5px solid var(--border);
                                   background: var(--bg2); }}
  .trial-head  {{ display: flex; flex-wrap: wrap; gap: 6px; align-items: center; }}
  .trial-title {{ font-size: 12px; color: var(--text); }}
  .trial-meta  {{ font-size: 10px; color: var(--text3); font-family: "SF Mono", monospace; }}
  .nct {{ color: var(--blue-600); text-decoration: none; font-weight: 600;
          font-family: "SF Mono", monospace; font-size: 11px;
          padding: 1px 6px; background: var(--blue-100); border-radius: 4px; }}
  .nct:hover {{ text-decoration: underline; }}
  .trial-body {{ padding: 10px 14px; font-size: 12px; }}
  .trial-body > div, .trial-body details {{ margin-bottom: 6px; }}
  .brief {{ color: var(--text2); }}
  .endpoint b, .interv b {{ color: var(--text); }}
  .endpoint, .interv {{ color: var(--text2); }}

  details.elig {{ margin-top: 6px; background: var(--bg2);
                  border-radius: 5px; padding: 4px 8px; font-size: 11px; }}
  details.elig > summary {{ cursor: pointer; color: var(--text2);
                            list-style: none; font-weight: 500; }}
  details.elig > summary::-webkit-details-marker {{ display: none; }}
  details.elig > summary::before {{ content: "▸ "; color: var(--text3); }}
  details.elig[open] > summary::before {{ content: "▾ "; }}
  .elig-body {{ padding: 6px 0; color: var(--text2); white-space: pre-wrap;
                font-family: -apple-system, BlinkMacSystemFont, sans-serif; }}

  .readout {{ background: #F0FAF6; border-left: 3px solid var(--teal-400);
              border-radius: 0 6px 6px 0; padding: 8px 12px; margin-bottom: 8px; }}
  .readout-head {{ font-size: 12px; font-weight: 600; color: var(--teal-900);
                   margin-bottom: 4px; }}
  .readout ul {{ margin: 4px 0 0; padding-left: 18px; font-size: 12px;
                 color: var(--text); }}
  .readout li {{ margin-bottom: 2px; }}

  /* Pills */
  .pill {{ display: inline-block; font-size: 9px; font-weight: 600;
           padding: 2px 7px; border-radius: 10px; white-space: nowrap;
           letter-spacing: 0.02em; }}
  /* phase */
  .ph-ph1   {{ background: var(--teal-100); color: var(--teal-900); }}
  .ph-ph12  {{ background: var(--teal-400); color: #fff; }}
  .ph-ph2   {{ background: var(--teal-600); color: #fff; }}
  .ph-ph23  {{ background: var(--teal-900); color: #fff; }}
  .ph-ph3   {{ background: #000; color: #fff; }}
  /* status */
  .st-active     {{ background: var(--teal-100); color: var(--teal-900); }}
  .st-planned    {{ background: var(--amber-100); color: var(--amber-600); }}
  .st-completed  {{ background: var(--blue-100); color: var(--blue-600); }}
  .st-terminated {{ background: var(--red-100); color: var(--red-800); }}
  /* biomarker */
  .bm-strict {{ background: #FAEEDA; color: var(--amber-600); }}
  .bm-soft   {{ background: var(--gray-100); color: var(--gray-900); }}
  .bm-none   {{ background: #FCEBEB; color: var(--red-800); }}
  /* combo */
  .cb-mono   {{ background: var(--bg3); color: var(--text2); }}
  .cb-chemo  {{ background: var(--gray-100); color: var(--gray-900); }}
  .cb-kras   {{ background: var(--purple-100); color: var(--purple-600); }}
  .cb-egfr   {{ background: var(--blue-100); color: var(--blue-600); }}
  .cb-io     {{ background: var(--teal-100); color: var(--teal-900); }}
  .cb-parp   {{ background: #FCEBEB; color: var(--red-800); }}
  .cb-other  {{ background: var(--gray-100); color: var(--gray-900); }}

  /* Cite chip */
  .cite {{ color: var(--teal-600); text-decoration: none; padding: 0 4px;
          border: 0.5px solid var(--teal-100); border-radius: 4px;
          background: #F0FAF6; font-size: 10px; font-weight: 600; }}
  .cite:hover {{ background: var(--teal-100); }}
  .cite.missing {{ color: #A32D2D; }}

  img.timeline {{ width: 100%; max-width: 1200px;
                  border: 0.5px solid var(--border); border-radius: 8px;
                  margin-bottom: 16px; background: white; }}
  .empty {{ color: var(--text3); font-weight: 400; font-size: 11px; }}
  .legend {{ font-size: 11px; color: var(--text2); margin: 0 0 16px;
              display: flex; gap: 10px; flex-wrap: wrap; align-items: center; }}
  .legend b {{ color: var(--text); margin-right: 4px; }}
</style>
</head>
<body>
  <div class="tpill">Panel 3 · MTAP-axis</div>
  <h1>Clinical landscape</h1>
  <div class="lead">
    {nrow(trials)} ClinicalTrials.gov entries across {uniqueN(trials$canonical_drug)} drugs in {length(classes)} mechanism classes.
    Click any drug to expand its trial list; click any trial to expand
    biomarker enrolment, combination partners, primary endpoint, and (where
    public) curated readouts from peer-reviewed and conference sources.
  </div>

  <div class="legend">
    <span><b>Biomarker:</b></span>
    <span class="pill bm-strict">MTAP / CDKN2A homdel required</span>
    <span class="pill bm-soft">MTAP reported, not required</span>
    <span class="pill bm-none">Not biomarker-selected</span>
    <span style="flex-basis: 100%; height: 0;"></span>
    <span><b>Combination:</b></span>
    <span class="pill cb-mono">Monotherapy</span>
    <span class="pill cb-kras">KRAS/RAS</span>
    <span class="pill cb-egfr">EGFR</span>
    <span class="pill cb-io">IO</span>
    <span class="pill cb-parp">PARP</span>
    <span class="pill cb-chemo">chemo</span>
    <span class="pill cb-other">other targeted</span>
  </div>

  <h2>Trial-activity timeline</h2>
  <img class="timeline" src="06_clinical_landscape.png" alt="MTAP-axis clinical trial timeline">

  {paste(class_blocks, collapse = "\n")}

</body>
</html>
', .open = "{", .close = "}")

out <- fs::path(RESULTS_DIR, "panel3_clinical_preview.html")
writeLines(html, out)
message(glue::glue("[render] {out}"))
