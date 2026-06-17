## Stage 0 — one-time fetch + cache of TCGA data shared across stages.
## For Stage 1 we only require:
##   - ABSOLUTE segment-level allelic CN (PanCanAtlas / Taylor 2018)
##   - ABSOLUTE sample-level purity / ploidy / QC table
## RNA-seq, MAF, GENIE come in later stages — kept out of this pull to save time.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fs)
  library(glue)
  library(stringr)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))

dir_create(CACHE_DIR)

download_if_missing <- function(url, dest, label) {
  if (file_exists(dest) && file_info(dest)$size > 1e5) {
    message(glue("[cache] {label}: present ({format(file_info(dest)$size, big.mark=',')} bytes)"))
    return(invisible(dest))
  }
  message(glue("[fetch] {label}  <-  {url}"))
  # curl handles long downloads + resume better than download.file default.
  rc <- system2("curl",
                args = c("-L", "--retry", "3", "--retry-delay", "5",
                         "--connect-timeout", "30", "--max-time", "1800",
                         "-o", shQuote(dest), shQuote(url)),
                stdout = "", stderr = "")
  if (rc != 0 || !file_exists(dest) || file_info(dest)$size < 1e5) {
    stop(glue("download failed (rc={rc}): {label}"))
  }
  message(glue("[ok]    {label}: {format(file_info(dest)$size, big.mark=',')} bytes"))
}

# ---------------------------------------------------------------------------
# 1) ABSOLUTE segment table
# ---------------------------------------------------------------------------
seg_raw  <- fs::path(CACHE_DIR, "absolute_segtabs.fixed.txt")
download_if_missing(ABSOLUTE_SEG_URL, seg_raw, "ABSOLUTE segments")

# ---------------------------------------------------------------------------
# 2) ABSOLUTE sample purity/ploidy/QC
# ---------------------------------------------------------------------------
pur_raw  <- fs::path(CACHE_DIR, "absolute_purity.txt")
download_if_missing(ABSOLUTE_PURITY_URL, pur_raw, "ABSOLUTE purity table")

# ---------------------------------------------------------------------------
# 3) PanCanAtlas sample QA — for aliquot_barcode → cancer type lookup
# ---------------------------------------------------------------------------
qa_raw <- fs::path(CACHE_DIR, "merged_sample_quality_annotations.tsv")
download_if_missing(PANCAN_SAMPLEQA_URL, qa_raw, "PanCanAtlas sample QA")

# ---------------------------------------------------------------------------
# Parse + normalize
# ---------------------------------------------------------------------------
message("[parse] reading ABSOLUTE segments...")
seg <- fread(seg_raw, sep = "\t", header = TRUE, na.strings = c("NA", ""))
setnames(seg, tolower(names(seg)))
# Expected columns: sample, chromosome, start, end, length, n_probes,
#                   modal_total_cn, modal_a1, modal_a2, ...
# Normalize chromosome to character without 'chr' prefix.
if ("chromosome" %in% names(seg)) seg[, chromosome := sub("^chr", "", as.character(chromosome))]
message(glue("[parse] segments: {nrow(seg)} rows, {uniqueN(seg$sample)} unique samples"))

message("[parse] reading ABSOLUTE purity/QC...")
pur <- fread(pur_raw, sep = "\t", header = TRUE, na.strings = c("NA", ""))
setnames(pur, tolower(names(pur)))
message(glue("[parse] purity table: {nrow(pur)} rows, cols = {paste(names(pur), collapse=', ')}"))

# Sample identifiers in pur:
#   - "array"  = full aliquot barcode (TCGA-XX-NNNN-NNT-NNX-NNNN-NN) — matches seg$sample
#   - "sample" = sample-level barcode (TCGA-XX-NNNN-NNT)
# We standardise on `sample` = the aliquot-level barcode to align with seg.
if ("sample" %in% names(pur)) setnames(pur, "sample", "sample_short")
sample_col <- intersect(c("array", "aliquot_barcode"), names(pur))[1]
if (is.na(sample_col)) stop("could not locate aliquot-barcode column in purity table")
setnames(pur, sample_col, "sample")

# Join cohort from PanCanAtlas sample QA file ------------------------------
message("[parse] reading PanCanAtlas sample QA...")
qa <- fread(qa_raw, sep = "\t", header = TRUE, na.strings = c("NA", ""))
setnames(qa, tolower(names(qa)))

# Pick the disease/cancer-type column and an aliquot barcode column
qa_disease_col <- intersect(c("cancer type", "cancer_type", "disease"), names(qa))[1]
qa_id_col      <- intersect(c("aliquot_barcode", "aliquot barcode", "aliquotbarcode",
                              "patient_barcode", "patient barcode", "barcode"), names(qa))[1]
if (is.na(qa_disease_col) || is.na(qa_id_col)) {
  stop(glue("PanCanAtlas QA missing expected columns; saw: {paste(names(qa), collapse=', ')}"))
}
setnames(qa, qa_disease_col, "cohort_short")
setnames(qa, qa_id_col,      "qa_barcode")

# Reduce QA to one row per patient (cancer type is patient-level).
qa[, patient := substr(qa_barcode, 1, 12)]
qa_patient <- unique(qa[, .(patient, cohort_short)])

pur[, patient := substr(sample, 1, 12)]
pur[, sample_type := substr(sample, 14, 15)]

pur <- merge(pur, qa_patient, by = "patient", all.x = TRUE)
pur[, cohort := ifelse(is.na(cohort_short), NA_character_,
                       ifelse(startsWith(cohort_short, "TCGA-"),
                              cohort_short, paste0("TCGA-", cohort_short)))]

# ABSOLUTE QC: typical column is 'call status' = "called" vs "non-aberrant" / "failed"
qc_col <- intersect(c("call status", "call_status", "solution"), names(pur))
if (length(qc_col)) {
  setnames(pur, qc_col[1], "qc_status")
  pur[, qc_pass := tolower(qc_status) %in% c("called", "called.", "pass", "passed")]
} else {
  pur[, qc_pass := TRUE]
  warning("no QC status column found; treating all samples as QC-pass")
}

# Save tidied parquet
seg_pq <- fs::path(CACHE_DIR, "absolute_segments.parquet")
pur_pq <- fs::path(CACHE_DIR, "absolute_samples.parquet")
write_parquet(seg, seg_pq)
write_parquet(pur, pur_pq)

# ---------------------------------------------------------------------------
# Verification: per-cohort sample counts
# ---------------------------------------------------------------------------
counts <- pur[sample_type == "01" & qc_pass == TRUE,
              .(n_primary_qc_pass = .N), by = cohort][order(-n_primary_qc_pass)]
message("\n[verify] per-cohort sample counts (primary tumor, ABSOLUTE QC pass):")
print(counts)
cat(glue("\n[verify] total cohorts in scope: {sum(counts$cohort %in% TCGA_PROJECTS)} / {length(TCGA_PROJECTS)}\n"))
cat(glue("[verify] total samples: {sum(counts$n_primary_qc_pass)}\n\n"))

# Audit
write_audit(
  audit_row(
    analysis     = "00_fetch_tcga",
    source       = "GDC PanCanAtlas ABSOLUTE (Taylor 2018)",
    n_in         = nrow(pur),
    n_excluded   = 0L,
    output_path  = paste(seg_pq, pur_pq, sep = "; "),
    notes        = glue("seg rows={nrow(seg)} ; samples={uniqueN(seg$sample)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 0 fetch complete.")
