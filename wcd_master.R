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

#' Create folder structures
setwd(my.dir)

#' Directories
dir.data <- file.path(my.dir, "data")
dir.results <- file.path(my.dir, "results")

#' Files
file.surveyspp <- "2009To2012CatchForSeminar.xlsx"
file.surveydata <- "FisheryIndices2015_Sablefish_V7.xlsx"
file.index <- "index.csv"

#' Inputs
verbose <- TRUE
# Species of interest and four overfished species.
# cowcod is excluded because rockfish conservation areas are successful in
# eliminating bycatch therefore they should not dictate fishermen's choice
# on whether or not they should switch gear.
species <- c("sablefish",
  "bocaccio", "darkblotched.rockfish", "Pacific.ocean.perch", "yelloweye.rockfish")

#' Packages
#' Set the CRAN mirror and install packages that are not currently installed
options("repos" = "http://R-Forge.R-project.org")
pckg.need <- c("censReg", "devtools", "ggmap", "knitr", "nwfscDeltaGLM", "xlsx")
pckg.have <- installed.packages()[, "Package"]
pckg.inst <- pckg.need[!pckg.need %in% pckg.have]
for (pckg in pckg.inst){
  if (pckg %in% c("nwfscDeltaGLM")) {
    devtools::install_github("nwfsc-assess/nwfscDeltaGLM", ref = "1.0.0")
  }
  install.packages(pckg, quiet = !verbose, verbose = verbose)
}
for (pckg in pckg.need) {
  library(pckg, character.only = TRUE, quietly = !verbose, verbose = verbose)
}

#' Risk pool measure
#' Using a literature search the following level of risk pool participation
#' per port group was generated.
risk <- c("Washington" = 4, "San Francisco and Bodega Bay" = 2,
  "Fort Bragg" = 4, "Monterey and Morro Bay" = 4)

#' Source working files
if (file.exists(file.path(dir.data, "wcd_data_raw.RData"))) {
  load(file.path(dir.data, "wcd_data_raw.RData"))
} else source("wcd_readdata.R")
source("wcd_survey.R")
