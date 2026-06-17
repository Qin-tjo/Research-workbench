## Stage 2a — fetch MTAP TPM across all 33 TCGA cohorts via recount3.
## Single-gene slice: we read full SE per project but keep only the MTAP row.
## Output: cache/rnaseq_mtap_tpm.parquet  (long: external_id, project, log2_tpm_plus1)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fs)
  library(glue)
  library(recount3)
  library(SummarizedExperiment)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))

out_pq <- fs::path(CACHE_DIR, "rnaseq_mtap_tpm.parquet")
if (file_exists(out_pq) && file_info(out_pq)$size > 1e4) {
  message(glue("[cache] {out_pq} exists; skipping recount3 fetch."))
  quit(save = "no")
}

# recount3 needs an HTTP cache dir; set to our cache.
options(recount3_url = "http://duffel.rail.bio/recount3")
Sys.setenv(RECOUNT3_CACHE = fs::path(CACHE_DIR, "recount3"))
dir_create(fs::path(CACHE_DIR, "recount3"))

# All 33 TCGA projects in recount3
projects <- recount3::available_projects(organism = "human")
tcga <- as.data.table(projects)[file_source == "tcga"]
message(glue("[recount3] {nrow(tcga)} TCGA projects available"))

# Helper to map MTAP -> Gencode v26 ENSG id used by recount3
mtap_ens <- "ENSG00000099810"   # MTAP, stable across Gencode versions

all_rows <- vector("list", length = nrow(tcga))

for (i in seq_len(nrow(tcga))) {
  proj <- tcga$project[i]
  short <- paste0("TCGA-", proj)
  if (!(short %in% TCGA_PROJECTS)) next   # skip FPPP, etc.
  message(glue("[{i}/{nrow(tcga)}] {short}"))
  rse <- tryCatch(
    recount3::create_rse(tcga[i], type = "gene", verbose = FALSE),
    error = function(e) { message("  failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(rse)) next

  # Match MTAP row by ENSG (strip version suffix)
  ens_clean <- sub("\\..*$", "", rownames(rse))
  row_idx <- which(ens_clean == mtap_ens)
  if (length(row_idx) == 0) {
    message("  MTAP not found in rowRanges")
    next
  }

  # recount3 ships raw base-coverage counts. Convert to read counts, then TPM:
  #   TPM_g = (read_counts_g / gene_length_g) / sum_j(read_counts_j / gene_length_j) * 1e6
  read_counts <- recount3::transform_counts(rse, by = "auc")   # base-cov -> read counts
  lens        <- as.numeric(rowData(rse)$bp_length)
  rate        <- sweep(read_counts, 1, lens, FUN = "/")        # counts / length
  rate_sum    <- colSums(rate, na.rm = TRUE)
  tpm_mat     <- sweep(rate, 2, rate_sum / 1e6, FUN = "/")     # divide by per-million scaling
  tpm_vec     <- tpm_mat[row_idx[1], ]

  meta <- as.data.table(as.data.frame(colData(rse)))
  # external_id is the GDC sample/aliquot barcode; recount3 uses tcga.tcga_barcode
  barcode_col <- intersect(c("tcga.tcga_barcode", "tcga_barcode", "external_id", "rail_id"),
                           names(meta))[1]
  rows <- data.table(
    project       = short,
    external_id   = as.character(meta[[barcode_col]]),
    rail_id       = as.character(meta$external_id),
    tpm           = as.numeric(tpm_vec)
  )
  rows[, log2_tpm_plus1 := log2(tpm + 1)]
  all_rows[[i]] <- rows
}

out <- rbindlist(all_rows, use.names = TRUE, fill = TRUE)
message(glue("[done] MTAP TPM rows: {nrow(out)} across {uniqueN(out$project)} projects"))
write_parquet(out, out_pq)

write_audit(
  audit_row(
    analysis    = "02b_fetch_rnaseq",
    source      = "recount3 (TCGA, gene-level TPM, MTAP only)",
    n_in        = nrow(out),
    n_excluded  = 0L,
    output_path = out_pq,
    notes       = glue("ENSG=ENSG00000099810; projects={uniqueN(out$project)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)
