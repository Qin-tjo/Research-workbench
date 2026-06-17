## Panel 2 — render the YAML content to an HTML preview snippet.
##
## Reads:
##   data/citations.yaml     — citation database
##   data/panel2_mechanism.yaml — structured panel content
## Writes:
##   results/panel2_mechanism_preview.html — standalone preview
##
## This renderer is intentionally simple. The final dashboard renderer (Panel 5
## work) will reuse the same citation-resolution logic but with the full HTML
## template framework.

suppressPackageStartupMessages({
  library(yaml)
  library(glue)
  library(fs)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))

DATA_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/data"
cit <- yaml::read_yaml(fs::path(DATA_DIR, "citations.yaml"))
pan <- yaml::read_yaml(fs::path(DATA_DIR, "panel2_mechanism.yaml"))

# Citation resolver: returns markdown-style superscript link list
cite_html <- function(keys) {
  if (is.null(keys) || length(keys) == 0) return("")
  parts <- vapply(keys, function(k) {
    c <- cit[[k]]
    if (is.null(c)) return(sprintf('<span class="cite missing">[%s?]</span>', k))
    short <- sub(",.*", "", c$authors)
    label <- sprintf('%s %d', short, c$year)
    sprintf('<a class="cite" href="%s" target="_blank" rel="noopener" title="%s">[%s]</a>',
            c$url, htmltools::htmlEscape(c$title), label)
  }, character(1))
  paste0('<sup class="cites">', paste(parts, collapse = " "), '</sup>')
}

# htmltools::htmlEscape isn't always available — fall back if needed
if (!requireNamespace("htmltools", quietly = TRUE)) {
  htmltools <- list(htmlEscape = function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  })
}

esc <- function(x) gsub("\\s+", " ", trimws(x))

# ----- HTML -----
flow_blocks <- vapply(pan$flow, function(s) {
  sprintf('
    <div class="step">
      <div class="step-num">%d</div>
      <div class="step-body">
        <div class="step-label">%s</div>
        <div class="step-text">%s %s</div>
      </div>
    </div>',
    s$id, esc(s$label), esc(s$text), cite_html(s$citations))
}, character(1))

biomarker_blocks <- vapply(pan$biomarker, function(b) {
  sprintf('
    <li>
      <div class="bm-point">%s</div>
      <div class="bm-detail">%s %s</div>
    </li>',
    esc(b$point), esc(b$detail), cite_html(b$citations))
}, character(1))

strategy_blocks <- vapply(pan$strategies, function(s) {
  sprintf('
    <div class="strat">
      <div class="strat-class">%s</div>
      <div class="strat-text">%s %s</div>
    </div>',
    esc(s$class), esc(s$rationale), cite_html(s$citations))
}, character(1))

uncert_blocks <- vapply(pan$uncertainties, function(u) {
  if (is.list(u)) {
    sprintf('<li>%s %s</li>', esc(u$text), cite_html(u$citations))
  } else {
    sprintf('<li>%s</li>', esc(u))
  }
}, character(1))

# Reference list of every key cited anywhere in this panel
all_keys <- unique(unlist(c(
  lapply(pan$flow, `[[`, "citations"),
  lapply(pan$biomarker, `[[`, "citations"),
  lapply(pan$strategies, `[[`, "citations"),
  lapply(pan$uncertainties, function(u) if (is.list(u)) u$citations else NULL)
)))
ref_blocks <- vapply(all_keys, function(k) {
  c <- cit[[k]]
  if (is.null(c)) return(sprintf('<li>[%s? — missing in citations.yaml]</li>', k))
  sprintf('
    <li id="ref-%s">
      <span class="ref-authors">%s</span>
      <span class="ref-title">%s.</span>
      <span class="ref-pub"><em>%s</em> (%d) %s; %s.</span>
      <a href="%s" target="_blank" rel="noopener" class="ref-link">PubMed</a>
    </li>',
    k, esc(c$authors), esc(c$title), c$journal, c$year,
    ifelse(is.null(c$volume), "", c$volume),
    ifelse(is.null(c$pages),  "", c$pages),
    c$url)
}, character(1))

html <- glue('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Panel 2 preview — {pan$title}</title>
<style>
  :root {{
    --teal-100:#9FE1CB; --teal-400:#1D9E75; --teal-600:#0F6E56; --teal-900:#04342C;
    --gray-50:#F1EFE8; --gray-100:#D3D1C7; --gray-400:#888780; --gray-900:#2C2C2A;
    --bg:#ffffff; --bg2:#f7f6f3; --border:rgba(0,0,0,0.10); --text:#1a1a18;
    --text2:#5f5e5a;
  }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
          background: var(--bg); color: var(--text); font-size: 14px;
          line-height: 1.55; margin: 0; padding: 28px 36px 60px;
          max-width: 1100px; }}
  h1 {{ font-size: 20px; margin: 0 0 4px; }}
  .tpill {{ display: inline-block; background: #E1F5EE; color: var(--teal-900);
            font-size: 10px; font-weight: 600; padding: 3px 10px; border-radius: 20px;
            margin-bottom: 8px; letter-spacing: 0.03em; text-transform: uppercase; }}
  .question {{ font-size: 12px; color: var(--text2); margin-bottom: 18px; }}
  .summary {{ background: var(--bg2); padding: 12px 16px; border-radius: 8px;
              border-left: 3px solid var(--teal-400); font-size: 13px;
              color: var(--text); margin-bottom: 28px; }}

  h2 {{ font-size: 14px; font-weight: 700; color: var(--text);
        border-bottom: 0.5px solid var(--border); padding-bottom: 6px;
        margin: 32px 0 14px; letter-spacing: 0.01em; }}

  .flow {{ display: grid; gap: 10px; }}
  .step {{ display: grid; grid-template-columns: 28px 1fr; gap: 10px;
           padding: 12px 14px; background: var(--bg2); border-radius: 8px;
           border: 0.5px solid var(--border); }}
  .step-num {{ width: 24px; height: 24px; border-radius: 50%; background: var(--teal-400);
               color: #fff; font-weight: 700; font-size: 11px; display: flex;
               align-items: center; justify-content: center; }}
  .step-label {{ font-size: 10px; font-weight: 700; color: var(--text2);
                 text-transform: uppercase; letter-spacing: 0.05em;
                 margin-bottom: 4px; }}
  .step-text {{ font-size: 13px; color: var(--text); }}

  ul.bm, ul.uncert {{ list-style: none; padding: 0; margin: 0; }}
  ul.bm li {{ padding: 10px 14px; border-left: 3px solid var(--teal-400);
              background: var(--bg2); border-radius: 0 6px 6px 0;
              margin-bottom: 8px; }}
  .bm-point {{ font-weight: 600; font-size: 13px; margin-bottom: 4px; }}
  .bm-detail {{ font-size: 12px; color: var(--text2); }}

  .strat {{ padding: 10px 14px; background: var(--bg2); border-radius: 8px;
            margin-bottom: 8px; border-left: 3px solid var(--gray-400); }}
  .strat-class {{ font-weight: 700; font-size: 12px; margin-bottom: 4px; }}
  .strat-text {{ font-size: 12px; color: var(--text2); }}

  ul.uncert li {{ padding: 8px 14px; background: #FAEEDA; color: #412402;
                  border-radius: 6px; margin-bottom: 6px; font-size: 12px; }}

  ol.refs {{ font-size: 12px; color: var(--text2); padding-left: 22px; }}
  ol.refs li {{ margin-bottom: 8px; }}
  .ref-authors {{ font-weight: 500; color: var(--text); }}
  .ref-title  {{ color: var(--text); margin-left: 4px; }}
  .ref-pub    {{ margin-left: 4px; }}
  .ref-link   {{ margin-left: 6px; color: var(--teal-600); text-decoration: none; }}
  .ref-link:hover {{ text-decoration: underline; }}

  sup.cites {{ font-size: 9px; line-height: 1; margin-left: 3px; }}
  a.cite {{ color: var(--teal-600); text-decoration: none; padding: 0 2px;
            border: 0.5px solid var(--teal-100); border-radius: 3px;
            background: #F0FAF6; font-size: 9px; font-weight: 600; }}
  a.cite:hover {{ background: var(--teal-100); }}
  .cite.missing {{ color: #A32D2D; }}
</style>
</head>
<body>
  <div class="tpill">Panel {pan$panel} · {pan$target}</div>
  <h1>{pan$title}</h1>
  <div class="question">{pan$question}</div>
  <div class="summary">{esc(pan$summary)}</div>

  <h2>Mechanism flow</h2>
  <div class="flow">
    {paste(flow_blocks, collapse = "\n")}
  </div>

  <h2>Biomarker rationale</h2>
  <ul class="bm">
    {paste(biomarker_blocks, collapse = "\n")}
  </ul>

  <h2>Therapeutic-strategy classes</h2>
  {paste(strategy_blocks, collapse = "\n")}

  <h2>Acknowledged uncertainties</h2>
  <ul class="uncert">
    {paste(uncert_blocks, collapse = "\n")}
  </ul>

  <h2>References (this panel)</h2>
  <ol class="refs">
    {paste(ref_blocks, collapse = "\n")}
  </ol>
</body>
</html>
', .open = "{", .close = "}")

out <- fs::path(RESULTS_DIR, "panel2_mechanism_preview.html")
writeLines(html, out)
message(glue::glue("[render] {out}"))
