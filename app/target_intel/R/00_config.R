## Target Intelligence pipeline — global config
## Single source of truth for gene symbol, GDC release, paths.

suppressPackageStartupMessages({
  library(fs)
  library(glue)
})

# --- Target ----------------------------------------------------------------
TARGET_GENE   <- "MTAP"
PARTNER_GENES <- c("CDKN2A", "CDKN2B")  # 9p21.3 neighborhood

# --- Cohort scope ----------------------------------------------------------
# All 33 TCGA cohorts; Stage 0 fetches each.
TCGA_PROJECTS <- c(
  "TCGA-ACC","TCGA-BLCA","TCGA-BRCA","TCGA-CESC","TCGA-CHOL","TCGA-COAD",
  "TCGA-DLBC","TCGA-ESCA","TCGA-GBM","TCGA-HNSC","TCGA-KICH","TCGA-KIRC",
  "TCGA-KIRP","TCGA-LAML","TCGA-LGG","TCGA-LIHC","TCGA-LUAD","TCGA-LUSC",
  "TCGA-MESO","TCGA-OV","TCGA-PAAD","TCGA-PCPG","TCGA-PRAD","TCGA-READ",
  "TCGA-SARC","TCGA-SKCM","TCGA-STAD","TCGA-TGCT","TCGA-THCA","TCGA-THYM",
  "TCGA-UCEC","TCGA-UCS","TCGA-UVM"
)

# --- Data sources ----------------------------------------------------------
# ABSOLUTE PanCanAtlas (Taylor et al. 2018) — pre-computed integer allelic CN.
# Distributed via GDC PanCanAtlas; URL is stable.
ABSOLUTE_SEG_URL <-
  "https://api.gdc.cancer.gov/data/0f4f5701-7b61-41ae-bda9-2805d1ca9781"
# File: TCGA_mastercalls.abs_segtabs.fixed.txt  (segment-level allelic CN)

ABSOLUTE_PURITY_URL <-
  "https://api.gdc.cancer.gov/data/4f277128-f793-4354-a13d-30cc7fe9f6b5"
# File: TCGA_mastercalls.abs_tables_JSedit.fixed.txt  (sample-level purity/ploidy/QC)

# PanCanAtlas merged sample quality annotations — gives aliquot_barcode → cancer type.
# Distributed via GDC; used purely as a cohort lookup.
PANCAN_SAMPLEQA_URL <-
  "https://api.gdc.cancer.gov/data/1a7d7be8-675d-4e60-a105-19d4121bdebf"
# File: merged_sample_quality_annotations.tsv

# Cancer driver gene whitelist — OncoKB public list (~1,240 genes spanning
# Vogelstein 2013, COSMIC CGC, MSK-IMPACT, FoundationOne, OncoKB curation).
# Used to filter Stage 3 partner pool, eliminating gene-length-bias artifacts.
ONCOKB_GENES_URL <-
  "https://www.oncokb.org/api/v1/utils/cancerGeneList.txt"

# Length-bias blocklist — genes notorious for false-positive cancer mutation
# signal due to extreme CDS length and no established driver role.
LENGTH_BIAS_BLOCKLIST <- c(
  "TTN", "MUC16", "MUC4", "MUC17", "MUC5B", "MUC12",
  "OBSCN", "FLG", "FLG2", "NEB",
  "RYR1", "RYR2", "RYR3",
  "DNAH5", "DNAH7", "DNAH9", "DNAH10", "DNAH11", "DNAH17",
  "AHNAK", "AHNAK2", "HMCN1", "HMCN2", "USH2A", "PCLO",
  "PCDH15", "FAT3", "CSMD1",   # CSMD3/FAT1 are real drivers — kept
  "MACF1", "PLEC", "SYNE1", "SYNE2", "FCGBP", "PKHD1"
)

GDC_DATA_RELEASE <- "PanCanAtlas-2018-Taylor"

# --- Paths -----------------------------------------------------------------
ROOT <- "/Users/qintjo/Documents/Research-workbench/app/target_intel"

CACHE_DIR   <- fs::path(ROOT, "cache")
RESULTS_DIR <- fs::path(ROOT, "results")
dir_create(CACHE_DIR); dir_create(RESULTS_DIR)

# --- Gene loci (GRCh37 / hg19) ---------------------------------------------
# IMPORTANT: ABSOLUTE PanCanAtlas segments (Taylor 2018) are aligned to hg19.
# Gene coordinates MUST be hg19 for segment-overlap logic to be correct.
# RNA-seq side (recount3, Gencode v26 / GRCh38) is matched by Ensembl gene ID,
# not by coordinates, so no build conflict there.
GENOME_BUILD <- "GRCh37"
GENE_LOCI <- data.frame(
  gene   = c("MTAP",   "CDKN2A", "CDKN2B"),
  chrom  = c("9",      "9",      "9"),
  start  = c(21802635, 21967751, 22002902),
  end    = c(21865969, 21994490, 22009362),
  stringsAsFactors = FALSE
)

# --- Misc ------------------------------------------------------------------
MIN_COHORT_N <- 20L     # Stage 1+ filter
PURITY_MIN   <- 0.40    # Stage 2 expression-fidelity filter

`%||%` <- function(a, b) if (is.null(a)) b else a
