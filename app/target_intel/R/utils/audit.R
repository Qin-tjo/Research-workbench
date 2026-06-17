## Audit log — every analysis appends a provenance row.

suppressPackageStartupMessages({
  library(arrow)
  library(fs)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

audit_row <- function(analysis, source, n_in, n_excluded, test = NA_character_,
                      adjust = NA_character_, output_path = NA_character_, notes = "") {
  data.frame(
    analysis        = analysis,
    source          = source,
    gdc_release     = if (exists("GDC_DATA_RELEASE")) GDC_DATA_RELEASE else NA_character_,
    r_version       = paste(R.version$major, R.version$minor, sep = "."),
    bioc_version    = tryCatch(as.character(BiocManager::version())[1], error = function(e) NA_character_) %||% NA_character_,
    query_date      = as.character(Sys.Date()),
    n_in            = as.integer(n_in),
    n_excluded      = as.integer(n_excluded),
    test            = test,
    adjust          = adjust,
    output_path     = output_path,
    git_sha         = tryCatch(suppressWarnings(system("git -C /Users/qintjo/Documents/Research-workbench rev-parse --short HEAD 2>/dev/null", intern = TRUE))[1], error = function(e) NA_character_) %||% NA_character_,
    notes           = notes,
    stringsAsFactors = FALSE
  )
}

write_audit <- function(rows, path) {
  if (file_exists(path)) {
    existing <- as.data.frame(read_parquet(path))
    rows <- rbind(existing, rows)
  }
  write_parquet(rows, path)
  message(glue("audit written: {path}  ({nrow(rows)} rows total)"))
}
