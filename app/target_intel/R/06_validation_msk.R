## Stage 5 — MTAP homozygous deletion in MSK-IMPACT (real-world cohort).
##
## Source: MSK-IMPACT 50K (Cancer Cell 2026, n = 54,331) via cBioPortal public
## API. CN calls are FACETS-derived panel CNA, GISTIC-thresholded; we read the
## −2 calls as homdel. Method is not directly comparable to TCGA ABSOLUTE
## allelic CN — this panel is its own descriptive view, not a head-to-head
## comparison.
##
## Output: per-indication MTAP homdel % (and MTAP+CDKN2A co-homdel %).

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(glue)
  library(fs)
  library(jsonlite)
  library(httr)
})

SCRIPT_DIR <- "/Users/qintjo/Documents/Research-workbench/app/target_intel/R"
source(file.path(SCRIPT_DIR, "00_config.R"))
source(file.path(SCRIPT_DIR, "utils/audit.R"))
source(file.path(SCRIPT_DIR, "utils/style.R"))

CBIOPORTAL_BASE <- "https://www.cbioportal.org/api"
MIN_MSK_N       <- 50L            # min samples in MSK indication
HOMDEL_VAL      <- -2L            # GISTIC threshold for homdel

# Pinned to avoid the moving-target effect of "latest"
MSK_STUDY_ID <- "msk_impact_50k_2026"
message(glue("[validation] using study: {MSK_STUDY_ID}"))

# ---------------------------------------------------------------------------
# Resolve CNA molecular profile + sample-list
# ---------------------------------------------------------------------------
mp_resp <- httr::GET(paste0(CBIOPORTAL_BASE, "/studies/", MSK_STUDY_ID,
                            "/molecular-profiles"),
                     httr::accept_json(), httr::timeout(120))
mp <- as.data.table(jsonlite::fromJSON(httr::content(mp_resp, "text",
                                                     encoding = "UTF-8")))
cna_prof <- mp[molecularAlterationType == "COPY_NUMBER_ALTERATION" &
               datatype == "DISCRETE"][1]
if (nrow(cna_prof) == 0) stop("no CNA discrete profile in ", MSK_STUDY_ID)
CNA_PROFILE_ID <- cna_prof$molecularProfileId
message(glue("[msk] CNA profile: {CNA_PROFILE_ID}"))

SAMPLE_LIST_ID <- paste0(MSK_STUDY_ID, "_all")

# ---------------------------------------------------------------------------
# Pull CNA for MTAP and CDKN2A
# ---------------------------------------------------------------------------
ENTREZ <- list(MTAP = 4507, CDKN2A = 1029)
cna_long_list <- list()
for (g in names(ENTREZ)) {
  body <- list(entrezGeneIds = list(ENTREZ[[g]]),
               sampleListId  = SAMPLE_LIST_ID)
  resp <- httr::POST(paste0(CBIOPORTAL_BASE, "/molecular-profiles/",
                            CNA_PROFILE_ID, "/molecular-data/fetch"),
                     body = body, encode = "json",
                     httr::accept_json(), httr::timeout(300))
  if (httr::http_error(resp))
    stop("CNA fetch failed for ", g, ": ", httr::http_status(resp)$message)
  dat <- as.data.table(jsonlite::fromJSON(httr::content(resp, "text",
                                                        encoding = "UTF-8")))
  if (nrow(dat) == 0) { message("[msk] no data for ", g); next }
  dat[, gene := g]
  cna_long_list[[g]] <- dat[, .(sampleId, patientId, gene, value)]
  message(glue("[msk] {g}: {nrow(dat)} samples"))
}
cna <- rbindlist(cna_long_list)
cna_wide <- dcast(cna, sampleId + patientId ~ gene, value.var = "value")
cna_wide[, mtap_homdel   := as.integer(MTAP   == HOMDEL_VAL)]
cna_wide[, cdkn2a_homdel := as.integer(CDKN2A == HOMDEL_VAL)]
cna_wide[, codel_homdel  := as.integer(mtap_homdel == 1 & cdkn2a_homdel == 1)]

# ---------------------------------------------------------------------------
# Pull cancer-type (OncoTree) for each sample
# ---------------------------------------------------------------------------
ct_resp <- httr::GET(paste0(CBIOPORTAL_BASE, "/studies/", MSK_STUDY_ID,
                            "/clinical-data?clinicalDataType=SAMPLE&projection=DETAILED"),
                     httr::accept_json(), httr::timeout(300))
clin <- as.data.table(jsonlite::fromJSON(httr::content(ct_resp, "text",
                                                       encoding = "UTF-8")))
clin_w <- dcast(clin[clinicalAttributeId %in% c("CANCER_TYPE", "ONCOTREE_CODE",
                                                 "CANCER_TYPE_DETAILED")],
                sampleId + patientId ~ clinicalAttributeId, value.var = "value")
setnames(clin_w, c("CANCER_TYPE", "ONCOTREE_CODE", "CANCER_TYPE_DETAILED"),
         c("cancer_type", "oncotree", "cancer_type_detailed"),
         skip_absent = TRUE)
message("[msk] clinical rows: ", nrow(clin_w))

merged <- merge(cna_wide, clin_w, by = c("sampleId", "patientId"), all.x = TRUE)

# ---------------------------------------------------------------------------
# Map OncoTree / CANCER_TYPE → TCGA cohort (so labels match Stages 1–4)
# ---------------------------------------------------------------------------
ONCOTREE_TO_TCGA <- c(
  PAAD = "TCGA-PAAD", PAAC = "TCGA-PAAD", PANET = "TCGA-PAAD",
  GBM  = "TCGA-GBM",  GBMNOS = "TCGA-GBM",
  BLCA = "TCGA-BLCA", BLAD = "TCGA-BLCA",
  LUAD = "TCGA-LUAD",
  LUSC = "TCGA-LUSC",
  HNSC = "TCGA-HNSC", HNL = "TCGA-HNSC", OPHSC = "TCGA-HNSC",
  SKCM = "TCGA-SKCM",
  MESO = "TCGA-MESO", PLMESO = "TCGA-MESO", PPM = "TCGA-MESO",
  BRCA = "TCGA-BRCA", IDC = "TCGA-BRCA", ILC = "TCGA-BRCA",
  COAD = "TCGA-COAD", READ = "TCGA-READ", CRC = "TCGA-COAD",
  PRAD = "TCGA-PRAD",
  STAD = "TCGA-STAD",
  ESCA = "TCGA-ESCA", EAC = "TCGA-ESCA", ESCC = "TCGA-ESCA",
  CESC = "TCGA-CESC",
  UCEC = "TCGA-UCEC",
  OV   = "TCGA-OV",   HGSOC = "TCGA-OV",
  KIRC = "TCGA-KIRC", CCRCC = "TCGA-KIRC",
  KIRP = "TCGA-KIRP", PRCC = "TCGA-KIRP",
  KICH = "TCGA-KICH",
  LIHC = "TCGA-LIHC",
  THCA = "TCGA-THCA",
  LGG  = "TCGA-LGG",  ASTR = "TCGA-LGG", ODG = "TCGA-LGG", OAST = "TCGA-LGG",
  SARC = "TCGA-SARC",
  CHOL = "TCGA-CHOL", IHCH = "TCGA-CHOL",
  ACC  = "TCGA-ACC",
  PCPG = "TCGA-PCPG",
  TGCT = "TCGA-TGCT",
  THYM = "TCGA-THYM",
  UVM  = "TCGA-UVM",
  DLBC = "TCGA-DLBC", DLBCL = "TCGA-DLBC", DLBCLNOS = "TCGA-DLBC",
  LAML = "TCGA-LAML", AML = "TCGA-LAML"
)
merged[, tcga := ONCOTREE_TO_TCGA[oncotree]]
ct_to_tcga <- function(ct) {
  if (is.na(ct)) return(NA_character_)
  ct <- tolower(ct)
  if (grepl("pancreatic", ct))                return("TCGA-PAAD")
  if (grepl("glioblastoma", ct))              return("TCGA-GBM")
  if (grepl("low.?grade glioma", ct))         return("TCGA-LGG")
  if (grepl("bladder", ct))                   return("TCGA-BLCA")
  if (grepl("non.?small cell lung",  ct)) {
    if (grepl("squamous", ct))                return("TCGA-LUSC")
    if (grepl("adenocarcinoma", ct))          return("TCGA-LUAD")
  }
  if (grepl("head and neck", ct))             return("TCGA-HNSC")
  if (grepl("mesothelioma", ct))              return("TCGA-MESO")
  if (grepl("melanoma", ct))                  return("TCGA-SKCM")
  if (grepl("breast", ct))                    return("TCGA-BRCA")
  if (grepl("colorectal|colon", ct))          return("TCGA-COAD")
  if (grepl("prostate", ct))                  return("TCGA-PRAD")
  if (grepl("stomach|gastric", ct))           return("TCGA-STAD")
  if (grepl("esophag", ct))                   return("TCGA-ESCA")
  if (grepl("cervic", ct))                    return("TCGA-CESC")
  if (grepl("endometrial|uterine", ct))       return("TCGA-UCEC")
  if (grepl("ovarian", ct))                   return("TCGA-OV")
  if (grepl("hepatocellular|liver", ct))      return("TCGA-LIHC")
  if (grepl("cholangiocarc|biliary", ct))     return("TCGA-CHOL")
  if (grepl("sarcoma", ct))                   return("TCGA-SARC")
  if (grepl("renal", ct))                     return("TCGA-KIRC")
  if (grepl("thyroid", ct))                   return("TCGA-THCA")
  if (grepl("thymo|thymic", ct))              return("TCGA-THYM")
  if (grepl("adrenocort", ct))                return("TCGA-ACC")
  if (grepl("pheochromo|paragan", ct))        return("TCGA-PCPG")
  if (grepl("germ cell|testicular", ct))      return("TCGA-TGCT")
  if (grepl("uveal", ct))                     return("TCGA-UVM")
  if (grepl("diffuse large", ct))             return("TCGA-DLBC")
  if (grepl("acute myeloid", ct))             return("TCGA-LAML")
  NA_character_
}
merged[is.na(tcga), tcga := vapply(cancer_type, ct_to_tcga, character(1))]

# Patient-level dedup — one row per patient (most-deleted call if multiple)
setorder(merged, patientId, -codel_homdel, -mtap_homdel)
merged <- merged[, .SD[1L], by = patientId]

# ---------------------------------------------------------------------------
# Per-indication summary
# ---------------------------------------------------------------------------
msk <- merged[!is.na(tcga),
  .(msk_n            = .N,
    msk_mtap_pct     = 100 * mean(mtap_homdel,   na.rm = TRUE),
    msk_cdkn2a_pct   = 100 * mean(cdkn2a_homdel, na.rm = TRUE),
    msk_codel_pct    = 100 * mean(codel_homdel,  na.rm = TRUE)),
  by = .(tcga)]
msk <- msk[msk_n >= MIN_MSK_N]
setorder(msk, -msk_mtap_pct)

out_pq <- fs::path(RESULTS_DIR, "05_msk_validation.parquet")
write_parquet(msk, out_pq)
print(msk)

# ---------------------------------------------------------------------------
# Bar plot — per-indication MTAP homdel% in MSK-IMPACT
# ---------------------------------------------------------------------------
msk[, cohort_label := paste0(sub("^TCGA-", "", tcga), "   n=", msk_n)]
msk[, cohort_label := factor(cohort_label, levels = rev(msk$cohort_label))]

p <- ggplot(msk, aes(x = msk_mtap_pct, y = cohort_label)) +
  geom_col(width = 0.78, fill = "#0F6E56") +
  geom_text(aes(x = msk_mtap_pct + 0.12,
                label = sprintf("homdel %.1f%% • co-del %.1f%%",
                                msk_mtap_pct, msk_codel_pct)),
            hjust = 0, size = 2.8, color = "#333333", family = "mono") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(msk$msk_mtap_pct) * 1.95),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(
    title    = "MTAP homozygous deletion in MSK-IMPACT 50K (panel-based)",
    subtitle = wrap_text(glue("Per indication, MTAP homdel% in the MSK-IMPACT public release ({MSK_STUDY_ID}, FACETS-derived GISTIC −2). Indications with ≥{MIN_MSK_N} samples shown. Right-margin label: MTAP homdel% • MTAP+CDKN2A co-homdel%."),
                          width = 140),
    x = "MTAP homdel %",
    y = NULL,
    caption  = wrap_text("MSK-IMPACT 50K (Cancer Cell 2026) is the largest publicly accessible real-world clinical-sequencing cohort on cBioPortal. CN calls are panel-based FACETS → GISTIC −2; panel coverage of MTAP varies by panel version and the cohort is enriched for metastatic / advanced disease.",
                          width = 150)
  ) +
  theme_target_intel(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(family = "mono")
  )

out_png <- fs::path(RESULTS_DIR, "05_msk_validation.png")
ggsave(out_png, p, width = 11,
       height = 0.34 * nrow(msk) + 2.8, dpi = 160)
message(glue("[plot] {out_png}"))

# Audit
write_audit(
  audit_row(
    analysis    = "05_msk_validation",
    source      = glue("MSK-IMPACT public release via cBioPortal ({MSK_STUDY_ID})"),
    n_in        = nrow(merged),
    n_excluded  = nrow(merged) - sum(!is.na(merged$tcga)),
    test        = "descriptive frequencies",
    adjust      = "none",
    output_path = paste(out_pq, out_png, sep = "; "),
    notes       = glue("indications_plotted={nrow(msk)}; panel_based=TRUE; population_enrichment=metastatic/advanced")
  ),
  fs::path(RESULTS_DIR, "audit.parquet")
)

message("[done] Stage 5 complete.")
