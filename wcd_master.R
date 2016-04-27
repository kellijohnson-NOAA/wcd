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
good <- c("portgrp", "year", "GEAR")
federalyear <- 1982
nastrings <- c("NA","NaN", " ", -999, "#N/A")

#' Directories
setwd(my.dir)
dir.data <- file.path(my.dir, "data")
dir.results <- file.path(my.dir, "results")

#' Files
file.surveyspp <- "JohnsonK_BocSablPopCowYeye_2003To2014.xlsx"
file.index <- "index.csv"
file.econ <- "econ4osuonlysablefish2009-2014.xlsx"
file.tac <- "wc_tac_v3.csv"
file.acl <- "pfmc_ACL_Sablefish.csv"
file.land <- "catchbyyearfleet.csv"
file.price <- "pacfin_LandingsByGear.csv"
file.script <- "saveAllTabsAsCSV.vbs"

ignore <- mapply(source, list.files("R", full.names = TRUE))

#' Inputs
verbose <- TRUE
my.spp <- c("sablefish") #could add overfished or DTS species
my.years <- 2009:2014
my.portgroups <- c("Washington", "Astoria and Tillamook",
  "Newport", "Coos Bay", "Brookings and Crescent City", "Eureka",
  "Fort Bragg", "San Francisco and Bodega Bay",
  "Monterey and Morro Bay")
portgrouporder <- c(9, 1, 7, 3, 2, 4, 5, 8, 6)
my.columns <- c(my.spp, "revenue", "buyercount",
  "Fixed.costs", "Variable.costs",
  "Crew", "Fuel", "Days", "TotRev", "VarCostNetRev", "TotCostNetRev",
  "FuelCapacity", "HorsePower", "Length")

# Plot
resolution <- 100
width <- 700

#' Packages
#' Set the CRAN mirror and install packages that are not currently installed
options("repos" = "http://R-Forge.R-project.org")
pckg.need <- c("betareg", "devtools", "factoextra",
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

#' Source working files
source("wcd_readdata.R")
source("wcd_survey.R")
source("wcd_readindex.R")
source("wcd_cluster.R")
source("wcd_map.R")
source("wcd_datamanipulation.R")
source("wcd_glm.R")
source("wcd_figures.R")
source("wcd_tables.R")
