###############################################################################
###############################################################################
## Purpose:    West coast drivers of fishermen choice
##             Read in the raw data
## Author:     Kelli Faye Johnson
## Contact:    kellifayejohnson@gmail.com
## Date:       2014-08-26
## Comments:
###############################################################################
###############################################################################

###############################################################################
#### Read data
###############################################################################
dir.data <- file.path(my.dir, "data")
dir.curr <- getwd()
setwd(dir.data)

## Survey data
fp.srvy <- file.path(dir.data, file.surveyspp)
# Partition out the csv file if it does not already exist.
filetotest <- gsub(".xlsx", paste0("_", "HaulWeight&Effort", ".csv"), fp.srvy)
if (!file.exists(filetotest)) {
  system(paste0("cscript \"", gsub("/", "\\\\", file.script),
    "\" \"", gsub("/", "\\\\", file.surveyspp), "\\"))
}

data.srvy <- read.csv(filetotest, skip = 9)

data.bio <- read.csv("trawl_surveys2.csv")

## Econ data
fp.econ <- file.path(dir.data, file.econ)
# Partition out the csv file if it does not already exist.
filetotest <- gsub(".xlsx", paste0("_", "specieslist", ".csv"), file.econ)
if (!file.exists(filetotest)) {
  system(paste0("cscript \"", gsub("/", "\\\\", file.script),
    "\" \"", gsub("/", "\\\\", file.econ), "\\"))
}

data.spp <- read.csv(
  gsub(".xlsx", paste0("_", "specieslist", ".csv"), file.econ),
  na.strings = nastrings)
data.vcount <- read.csv(
  gsub(".xlsx", paste0("_", "vesseldeliverycount", ".csv"), file.econ),
  na.strings = nastrings)
data.bcount <- read.csv(
  gsub(".xlsx", paste0("_", "fishbuyercount", ".csv"), file.econ),
  na.strings = nastrings)
data.rev <- read.csv(
  gsub(".xlsx", paste0("_", "Revenue", ".csv"), file.econ),
  na.strings = nastrings)
data.land <- read.csv(
  gsub(".xlsx", paste0("_", "Landings", ".csv"), file.econ),
  na.strings = nastrings)
data.vess <- read.csv(
  gsub(".xlsx", paste0("_", "VesselCharacteristics", ".csv"), file.econ),
  na.strings = nastrings)
data.days <- read.csv(
  gsub(".xlsx", paste0("_", "dayscrewfuelspeeddays", ".csv"),
  file.econ), stringsAsFactors = FALSE,
  na.strings = nastrings)
data.cost <- read.csv(
  gsub(".xlsx", paste0("_", "Costs", ".csv"), file.econ),
  na.strings = nastrings)
data.netrev <- read.csv(
  gsub(".xlsx", paste0("_", "NetRevenue", ".csv"), file.econ),
  na.strings = nastrings)

#' Fix "year" column name
colnames(data.cost)[1] <- tolower(colnames(data.cost))[1]
colnames(data.days)[1] <- tolower(colnames(data.days))[1]
colnames(data.netrev)[1] <- tolower(colnames(data.netrev))[1]
colnames(data.vess)[1] <- tolower(colnames(data.vess))[1]
colnames(data.spp)[which(colnames(data.spp) == "YEAR")] <-
  tolower(colnames(data.spp))[which(colnames(data.spp) == "YEAR")]
colnames(data.rev) <- gsub("^X", "", colnames(data.rev))
colnames(data.bcount) <- gsub("^X", "", colnames(data.bcount))
colnames(data.land) <- gsub("^X", "", colnames(data.land))

#' read in index of abundance data
data.index <- read.csv(file.path(dir.results, file.index))
data.indexmatch <- do.call("rbind",
  lapply(1:length(my.years), function(x) {
  data.frame(
  "portgrp" = c("Astoria and Tillamook",
    "Brookings and Crescent City",
    "Coos Bay", "Eureka", "Fort Bragg", "Newport",
    "San Francisco and Bodega Bay", "Washington"),
  "top" = c("A", "B", "B", "C", "C", "A", "C", "A"),
  "bot" = c("B", "C", "C", "D", "D", "B", "D", "B"),
  "year" = my.years[x])
}))
data.indexmatch <- Reduce(function(...) merge(..., all = TRUE),
  lapply(3:6, function(x) {
    a <- merge(data.indexmatch, data.index[, c(1, 2, x)],
      by.x = c("top", "year"), by.y = c("strat", "year"))
    a <- a[, -which(colnames(a) %in% c("top", "bot"))]
    b <- merge(data.indexmatch, data.index[, c(1:x)],
      by.x = c("bot", "year"), by.y = c("strat", "year"))
    a[, NCOL(a)] <-
      a[, which(colnames(a) == colnames(data.index)[x])] +
      b[, which(colnames(b) == colnames(data.index)[x])]
    return(a)
}))

# TAC data
fp.alloc <- file.path(dir.data, file.alloc)
filetotest <- gsub(".xlsx", paste0("_", "after", ".csv"), fp.alloc)
if (!file.exists(filetotest)) {
  system(paste0("cscript \"", gsub("/", "\\\\", file.script),
    "\" \"", gsub("/", "\\\\", fp.alloc), "\\"))
}
data.tac.after <- read.csv(filetotest)
data.tac.before <- read.csv(file.path(dir.data, file.tac))
data.acl <- read.csv(file.acl)
data.landbygear <- read.csv(file.land)

###############################################################################
#### Save data in data dir and return to old working directory upon exit
###############################################################################
save(list = ls(pattern = "data"), file = "wcd_data_raw.RData")
setwd(dir.curr)

#endOfFile
