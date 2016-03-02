###############################################################################
###############################################################################
## Purpose:    West coast drivers of fishermen choice
## Author:     Kelli Faye Johnson
## Contact:    kellifayejohnson@gmail.com
## Date:       2014-08-26
## Comments:
###############################################################################
###############################################################################

#' Set variable inputs
my.dir <- "c:/wcd"
if (!file.exists(my.dir)) my.dir <- "d:/wcd"
portgrouporder <- c(9, 1, 7, 3, 2, 4, 5, 8, 6)
good <- c("portgrp", "year", "GEAR")
federalyear <- 1982
nastrings <- c("NA","NaN", " ", -999, "#N/A")

#' Directories
setwd(my.dir)
dir.data <- file.path(my.dir, "data")
dir.results <- file.path(my.dir, "results")

#' Files
file.surveyspp <- "JohnsonK_BocSablPopCowYeye_2003To2014.xlsx"
file.surveydata <- "FisheryIndices2015_Sablefish_V7.xlsx"
file.index <- "index.csv"
file.econ <- "econ4osu2009-2013.xlsx"
file.alloc <- "wcd_allocation.xlsx"
file.tac <- "wc_tac_v3.csv"
file.aclall <- "pfmc_ACL_2003-2016_Sheet1.csv"
file.acl <- "pfmc_ACL_Sablefish.csv"
file.land <- "catchbyyearfleet.csv"
file.script <- file.path(dir.data, "saveAllTabsAsCSV.vbs")
ignore <- mapply(source, list.files("R", full.names = TRUE))

#' Inputs
verbose <- TRUE
# Species of interest and four overfished species.
# exclude cowcod bc RCAs are successful in eliminating bycatch
my.spp <- c("sablefish",
  "bocaccio", "darkblotched.rockfish", "Pacific.ocean.perch", "yelloweye.rockfish")
my.years <- 2009:2013
my.portgroups <- c("Washington", "Astoria and Tillamook",
  "Newport", "Coos Bay", "Brookings and Crescent City", "Eureka",
  "Forg Bragg", "San Francisco and Bodega Bay")

# Plot
resolution <- 100
width <- 700
height <- width

#' Packages
#' Set the CRAN mirror and install packages that are not currently installed
options("repos" = "http://R-Forge.R-project.org")
pckg.need <- c("devtools", "factoextra", "gamboostLSS", "gamlss",
  "ggdendro", "ggmap", "ggplot2", "grid",
  "knitr", "nwfscDeltaGLM", "raster", "vegan", "xtable")
for (pckg in pckg.need[!pckg.need %in% installed.packages()[, "Package"]]){
  if (pckg %in% c("nwfscDeltaGLM")) {
    devtools::install_github("nwfsc-assess/nwfscDeltaGLM", ref = "1.0.0")
  }
  install.packages(pckg, quiet = !verbose, verbose = verbose)
}
for (pckg in pckg.need) {
  library(pckg, character.only = TRUE, quietly = !verbose, verbose = verbose)
}

#' Create ggplot theme
theme <-   theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7, face = "bold")
  )

#' Risk pool measure
#' Using a literature search the following level of risk pool participation
#' per port group was generated.
risk <- c("Washington" = 4, "San Francisco and Bodega Bay" = 2,
  "Fort Bragg" = 4, "Monterey and Morro Bay" = 4)

#' Source working files
source("wcd_readdata.R")
source("wcd_survey.R")
source("wcd_readindex.R")
source("wcd_cluster.R")
source("wcd_map.R")
source("wcd_datamanipulation.R")
source("wcd_gamboostLSS.R")
source("wcd_figures.R")
