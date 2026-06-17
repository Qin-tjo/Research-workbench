## Stage 3a — fetch MC3 PanCanAtlas public MAF.
## Source: Ellrott et al. 2018; UUID points to mc3.v0.2.8.PUBLIC.maf.gz (~600 MB).

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fs)
  library(glue)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))

# MC3 v0.2.8 PUBLIC MAF — pre-computed pan-TCGA somatic call set
MC3_URL <- "https://api.gdc.cancer.gov/data/1c8cfe5f-e52d-41ba-94da-f15ea1337efc"

raw_gz <- fs::path(CACHE_DIR, "mc3.v0.2.8.PUBLIC.maf.gz")
parq   <- fs::path(CACHE_DIR, "mc3_public.parquet")

if (file_exists(parq) && file_info(parq)$size > 1e6) {
  message(glue("[cache] {parq} present — skipping MAF parse"))
  quit(save = "no")
}

if (!file_exists(raw_gz) || file_info(raw_gz)$size < 1e8) {
  message(glue("[fetch] MC3 public MAF  <-  {MC3_URL}"))
  rc <- system2("curl",
                args = c("-L", "--retry", "3", "--retry-delay", "5",
                         "--connect-timeout", "30", "--max-time", "3600",
                         "-o", shQuote(raw_gz), shQuote(MC3_URL)),
                stdout = "", stderr = "")
  if (rc != 0) stop("MC3 download failed")
}
message(glue("[ok]    MC3 file: {format(file_info(raw_gz)$size, big.mark=',')} bytes"))

# Read only the columns we need. fread handles .gz natively.
keep_cols <- c("Hugo_Symbol", "Variant_Classification", "Variant_Type",
               "Tumor_Sample_Barcode")
message("[parse] reading MC3 MAF (this takes ~1-2 min)...")
maf <- fread(cmd = paste("gunzip -c", shQuote(raw_gz)),
             sep = "\t", header = TRUE, na.strings = c("", "NA", "."),
             select = keep_cols, quote = "")
setnames(maf, tolower(names(maf)))
message(glue("[parse] mutations: {nrow(maf)} rows; samples: {uniqueN(maf$tumor_sample_barcode)}"))

# Derive patient id (12-char) and short sample id (15-char)
maf[, patient := substr(tumor_sample_barcode, 1, 12)]
maf[, sample15 := substr(tumor_sample_barcode, 1, 15)]

# Non-silent mutation flag (used by Stage 3)
nonsilent_classes <- c("Missense_Mutation", "Nonsense_Mutation",
                       "Frame_Shift_Del", "Frame_Shift_Ins",
                       "Splice_Site", "Translation_Start_Site",
                       "Nonstop_Mutation", "In_Frame_Del", "In_Frame_Ins")
maf[, nonsilent := variant_classification %in% nonsilent_classes]

write_parquet(maf, parq)
message(glue("[ok]    wrote {parq}"))

write_audit(
  audit_row(
    analysis    = "03b_fetch_maf",
    source      = "MC3 v0.2.8 PUBLIC (Ellrott 2018)",
    n_in        = nrow(maf),
    n_excluded  = 0L,
    output_path = parq,
    notes       = glue("samples={uniqueN(maf$tumor_sample_barcode)}; nonsilent={sum(maf$nonsilent)}")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)
