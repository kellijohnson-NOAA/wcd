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
#' Inputs
#' Packages
#' Set the CRAN mirror and install packages that are not currently installed
options("repos" = "http://R-Forge.R-project.org")
