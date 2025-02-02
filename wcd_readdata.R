###############################################################################
###############################################################################
## Purpose:    West coast drivers of fishermen choice
##             Read in the raw data
## Author:     Kelli Faye Johnson
## Contact:    kellifayejohnson@gmail.com
## Date:       2016-03-03
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
fp.script <- file.path(dir.data, file.script)
# Partition out the csv file if it does not already exist.
filetotest <- gsub(".xlsx", paste0("_", "HaulWeight&Effort", ".csv"), fp.srvy)
if (!file.exists(filetotest)) {
  system(paste0("cscript \"", gsub("/", "\\\\", fp.script),
    "\" \"", gsub("/", "\\\\", file.surveyspp), "\\"))
}

data.srvy <- read.csv(filetotest, skip = 9)

## Econ data
fp.econ <- file.path(dir.data, file.econ)
# Partition out the csv file if it does not already exist.
filetotest <- gsub(".xlsx", paste0("_", "specieslist", ".csv"), file.econ)
if (!file.exists(filetotest)) {
  system(paste0("cscript \"", gsub("/", "\\\\", fp.script),
    "\" \"", gsub("/", "\\\\", file.econ), "\\"))
}

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
colnames(data.rev) <- gsub("^X", "", colnames(data.rev))
colnames(data.bcount) <- gsub("^X", "", colnames(data.bcount))
colnames(data.land) <- gsub("^X", "", colnames(data.land))

# TAC data
data.acl <- read.csv(file.acl)

###############################################################################
#### Read in the catch data as provided for the stock assessment
#### Catches are grouped according to gear that was used while fishing
#### not permit type held while fishing.
###############################################################################
data.landbygear <- read.csv(file.land)
catch <- aggregate(catch ~ year + fleet, data = data.landbygear, sum)
catch <- reshape(catch, direction = "wide", timevar = "fleet", idvar = "year")
colnames(catch) <- gsub("catch\\.", "", colnames(catch))
rownames(catch) <- catch$year

#' Change absolute catches into proportion of the total catch for each gear
props <- data.frame("year" = catch[, 1],
  prop.table(as.matrix(catch[, -1]), margin = 1))

###############################################################################
#### Read in the price data for all pacfin species
###############################################################################
data.price <- read.csv(file.path(dir.data, file.price))

###############################################################################
#### Return to old working directory upon exit
###############################################################################
setwd(dir.curr)

#endOfFile
