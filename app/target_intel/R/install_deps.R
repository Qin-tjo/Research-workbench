options(repos = c(CRAN = "https://cloud.r-project.org"))

cran_pkgs <- c("arrow", "data.table", "dplyr", "tidyr", "readr", "ggplot2",
               "scales", "stringr", "purrr", "jsonlite", "fs", "glue", "renv",
               "BiocManager")

to_install <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, Ncpus = parallel::detectCores())

bioc_pkgs <- c("TCGAbiolinks", "maftools", "cBioPortalData", "SummarizedExperiment")
to_install_bioc <- setdiff(bioc_pkgs, rownames(installed.packages()))
if (length(to_install_bioc)) BiocManager::install(to_install_bioc, ask = FALSE, update = FALSE, Ncpus = parallel::detectCores())

cat("\n---installed---\n")
print(installed.packages()[c(cran_pkgs, bioc_pkgs)[c(cran_pkgs, bioc_pkgs) %in% rownames(installed.packages())], "Version"])
