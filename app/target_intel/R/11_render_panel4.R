## Panel 4 — render the synthesis / thesis YAML to an HTML preview.

suppressPackageStartupMessages({
  library(yaml)
  library(glue)
  library(fs)
  library(htmltools)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))

DATA_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"
cit <- yaml::read_yaml(fs::path(DATA_DIR, "citations.yaml"))
pan <- yaml::read_yaml(fs::path(DATA_DIR, "panel4_synthesis.yaml"))

esc <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("")
  htmltools::htmlEscape(as.character(x))
}
nz <- function(x) !is.null(x) && length(x) >= 1 && nzchar(trimws(as.character(x)[1]))

cite_chip <- function(key) {
  c <- cit[[key]]
  if (is.null(c)) return(sprintf('<span class="cite missing">[%s?]</span>', esc(key)))
  short <- sub(",.*", "", c$authors)
  sprintf('<a class="cite" href="%s" target="_blank" rel="noopener" title="%s">%s %d</a>',
          c$url, esc(c$title), esc(short), c$year)
}
cite_html <- function(keys) {
  if (is.null(keys) || length(keys) == 0) return("")
  parts <- vapply(keys, cite_chip, character(1))
  paste0('<sup class="cites">', paste(parts, collapse = " "), '</sup>')
}

panel_chip <- function(p) sprintf('<a class="panel-ref" href="#panel-%s">%s</a>',
                                   gsub("[^A-Za-z0-9]", "", p), esc(p))
panels_html <- function(panels) {
  if (is.null(panels) || length(panels) == 0) return("")
  paste0('<span class="panels">',
         paste(vapply(panels, panel_chip, character(1)), collapse = " "),
         '</span>')
}

# --- Render ----------------------------------------------------------------
ps_blocks <- vapply(pan$patient_selection, function(p) {
  refs <- if (is.list(p$refs)) paste(panels_html(p$refs$panels),
                                     cite_html(p$refs$citations)) else ""
  sprintf('
    <li>
      <div class="ps-point">%s</div>
      <div class="ps-detail">%s</div>
      <div class="ps-refs">%s</div>
    </li>',
    esc(p$point), esc(p$detail), refs)
}, character(1))

combo_blocks <- vapply(pan$combinations, function(co) {
  refs <- if (is.list(co$refs)) paste(panels_html(co$refs$panels),
                                      cite_html(co$refs$citations)) else ""
  strats <- paste(sprintf('<li>%s</li>',
                           vapply(co$leading_strategies, esc, character(1))),
                   collapse = "")
  sprintf('
    <div class="indication">
      <div class="ind-head">
        <span class="ind-name">%s</span>
        <span class="ind-prev">prevalence: %s</span>
      </div>
      <ul class="strats">%s</ul>
      <div class="ind-rat">%s</div>
      <div class="ind-refs">%s</div>
    </div>',
    esc(co$indication), esc(co$mtap_prev_tcga),
    strats, esc(co$rationale), refs)
}, character(1))

prog_blocks <- vapply(pan$competitive$programs, function(p) {
  sprintf('
    <div class="prog">
      <div class="prog-head"><b>%s</b> — <span class="prog-drug">%s</span></div>
      <div class="prog-pos">%s</div>
      <div class="prog-cat"><b>Catalysts:</b> %s</div>
    </div>',
    esc(p$sponsor), esc(p$drug), esc(p$position), esc(p$catalysts))
}, character(1))

risk_blocks <- vapply(pan$risks, function(r) {
  refs <- {
    panel_part <- if (is.list(r$refs)) panels_html(r$refs$panels) else ""
    cite_part  <- cite_html(r$citations %||% (if (is.list(r$refs)) r$refs$citations else NULL))
    paste(panel_part, cite_part)
  }
  sprintf('
    <li>
      <div class="risk-point">%s</div>
      <div class="risk-detail">%s</div>
      <div class="risk-refs">%s</div>
    </li>',
    esc(r$point), esc(r$detail), refs)
}, character(1))

step_blocks <- vapply(pan$next_steps, function(s) {
  refs <- cite_html(s$citations %||% NULL)
  sprintf('
    <li>
      <div class="step-act">%s</div>
      <div class="step-why">%s %s</div>
    </li>',
    esc(s$action), esc(s$why), refs)
}, character(1))

# Reference roundup
all_keys <- unique(unlist(c(
  pan$thesis$citations,
  lapply(pan$patient_selection, function(p) {
    c(p$refs$citations %||% NULL, p$citations %||% NULL)
  }),
  lapply(pan$combinations, function(co) {
    c(co$refs$citations %||% NULL, co$citations %||% NULL)
  }),
  lapply(pan$risks, function(r) {
    c(r$refs$citations %||% NULL, r$citations %||% NULL)
  }),
  lapply(pan$next_steps, function(s) s$citations %||% NULL)
)))

ref_blocks <- vapply(all_keys, function(k) {
  c <- cit[[k]]
  if (is.null(c)) return(sprintf('<li>[%s? — missing in citations.yaml]</li>', k))
  sprintf('
    <li id="ref-%s">
      <span class="ref-authors">%s</span>
      <span class="ref-title">%s.</span>
      <span class="ref-pub"><em>%s</em> (%d) %s; %s.</span>
      <a href="%s" target="_blank" rel="noopener" class="ref-link">link</a>
    </li>',
    k, esc(c$authors), esc(c$title), c$journal, c$year,
    esc(c$volume %||% ""), esc(c$pages %||% ""), c$url)
}, character(1))

html <- glue('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Panel 4 — {pan$title}</title>
<style>
  :root {{
    --teal-100:#9FE1CB; --teal-400:#1D9E75; --teal-600:#0F6E56; --teal-900:#04342C;
    --gray-50:#F1EFE8; --gray-100:#D3D1C7; --gray-400:#888780; --gray-900:#2C2C2A;
    --amber-100:#FAC775; --amber-600:#854F0B; --amber-900:#412402;
    --red-100:#F7C1C1; --red-800:#791F1F;
    --blue-100:#B5D4F4; --blue-600:#185FA5;
    --purple-100:#CECBF6; --purple-600:#534AB7; --purple-900:#26215C;
    --bg:#ffffff; --bg2:#f7f6f3; --bg3:#f0efe9;
    --border:rgba(0,0,0,0.10); --text:#1a1a18; --text2:#5f5e5a; --text3:#888780;
  }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
          background: var(--bg); color: var(--text); font-size: 14px;
          line-height: 1.6; margin: 0; padding: 28px 36px 60px; max-width: 1080px; }}
  h1 {{ font-size: 22px; margin: 0 0 4px; }}
  .tpill {{ display: inline-block; background: #E1F5EE; color: var(--teal-900);
            font-size: 10px; font-weight: 600; padding: 3px 10px; border-radius: 20px;
            margin-bottom: 8px; letter-spacing: 0.03em; text-transform: uppercase; }}
  .question {{ font-size: 12px; color: var(--text2); margin: 0 0 22px; }}
  h2 {{ font-size: 14px; font-weight: 700; color: var(--text);
        border-bottom: 0.5px solid var(--border); padding-bottom: 6px;
        margin: 32px 0 14px; letter-spacing: 0.01em; }}

  .thesis {{ background: linear-gradient(180deg, #F0FAF6 0%, var(--bg2) 100%);
              border: 0.5px solid var(--teal-100); border-radius: 10px;
              padding: 16px 18px; margin-bottom: 14px; font-size: 14px;
              line-height: 1.65; }}
  .thesis .refs {{ font-size: 11px; color: var(--text3); margin-top: 8px; }}

  ul.ps, ul.risk, ul.steps {{ list-style: none; padding: 0; margin: 0; }}
  ul.ps li {{ padding: 11px 14px; border-left: 3px solid var(--teal-400);
              background: var(--bg2); border-radius: 0 6px 6px 0;
              margin-bottom: 10px; }}
  ul.risk li {{ padding: 11px 14px; border-left: 3px solid var(--amber-600);
                 background: #FAEEDA; border-radius: 0 6px 6px 0;
                 margin-bottom: 10px; }}
  ul.steps li {{ padding: 11px 14px; border-left: 3px solid var(--purple-600);
                 background: #EEEDFE; border-radius: 0 6px 6px 0;
                 margin-bottom: 10px; }}
  .ps-point, .risk-point, .step-act {{ font-weight: 700; margin-bottom: 4px; }}
  .ps-detail, .risk-detail {{ font-size: 12px; color: var(--text2); }}
  .step-why {{ font-size: 12px; color: var(--text2); }}
  .ps-refs, .risk-refs, .ind-refs {{ font-size: 10px; color: var(--text3); margin-top: 6px; }}

  .indication {{ background: var(--bg2); border-radius: 8px;
                  padding: 11px 14px; margin-bottom: 10px;
                  border: 0.5px solid var(--border); }}
  .ind-head  {{ display: flex; align-items: baseline; gap: 10px; margin-bottom: 4px; }}
  .ind-name  {{ font-weight: 700; font-size: 13px; }}
  .ind-prev  {{ font-size: 10px; color: var(--text3); font-family: "SF Mono", monospace; }}
  .strats    {{ margin: 6px 0; padding-left: 18px; }}
  .strats li {{ font-size: 12px; color: var(--text); }}
  .ind-rat   {{ font-size: 12px; color: var(--text2); margin-top: 4px;
                 padding: 6px 10px; background: var(--bg); border-radius: 5px; }}

  .prog {{ background: var(--bg2); border-radius: 8px; padding: 10px 14px;
            margin-bottom: 8px; border-left: 3px solid var(--blue-600); }}
  .prog-head {{ font-size: 13px; }}
  .prog-drug {{ font-family: "SF Mono", monospace; color: var(--blue-600); }}
  .prog-pos {{ font-size: 12px; color: var(--text2); margin: 3px 0; }}
  .prog-cat {{ font-size: 11px; color: var(--text2); }}

  sup.cites {{ font-size: 9px; line-height: 1; margin-left: 3px; }}
  a.cite {{ color: var(--teal-600); text-decoration: none; padding: 0 4px;
            border: 0.5px solid var(--teal-100); border-radius: 3px;
            background: #F0FAF6; font-size: 9px; font-weight: 600; }}
  a.cite:hover {{ background: var(--teal-100); }}
  .cite.missing {{ color: #A32D2D; }}
  .panels {{ margin-right: 6px; }}
  a.panel-ref {{ display: inline-block; font-size: 9px; font-weight: 600;
                  padding: 1px 6px; background: var(--purple-100);
                  color: var(--purple-900); border-radius: 3px;
                  text-decoration: none; margin-right: 3px; }}
  a.panel-ref:hover {{ background: var(--purple-600); color: #fff; }}

  ol.refs {{ font-size: 11px; color: var(--text2); padding-left: 22px; }}
  ol.refs li {{ margin-bottom: 7px; }}
  .ref-authors {{ font-weight: 500; color: var(--text); }}
  .ref-title  {{ color: var(--text); margin-left: 4px; }}
  .ref-pub    {{ margin-left: 4px; }}
  .ref-link   {{ margin-left: 6px; color: var(--teal-600); text-decoration: none; }}
  .ref-link:hover {{ text-decoration: underline; }}
</style>
</head>
<body>
  <div class="tpill">Panel {pan$panel} · {pan$target}</div>
  <h1>{pan$title}</h1>
  <div class="question">{pan$question}</div>

  <h2>Thesis</h2>
  <div class="thesis">
    {esc(pan$thesis$text)}
    <div class="refs">{cite_html(pan$thesis$citations)}</div>
  </div>

  <h2>Patient-selection strategy</h2>
  <ul class="ps">{paste(ps_blocks, collapse = "\n")}</ul>

  <h2>Combination-therapy logic — by indication</h2>
  {paste(combo_blocks, collapse = "\n")}

  <h2>Competitive landscape & catalysts</h2>
  {paste(prog_blocks, collapse = "\n")}

  <h2>Key risks & open questions</h2>
  <ul class="risk">{paste(risk_blocks, collapse = "\n")}</ul>

  <h2>Worth exploring next</h2>
  <ul class="steps">{paste(step_blocks, collapse = "\n")}</ul>

  <h2>References (this panel)</h2>
  <ol class="refs">{paste(ref_blocks, collapse = "\n")}</ol>
</body>
</html>
', .open = "{", .close = "}")

out <- fs::path(RESULTS_DIR, "panel4_synthesis_preview.html")
writeLines(html, out)
message(glue::glue("[render] {out}"))
