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

data.spp <- read.csv(gsub(".xlsx", paste0("_", "specieslist", ".csv"), file.econ))
data.vcount <- read.csv(gsub(".xlsx", paste0("_", "vesseldeliverycount", ".csv"), file.econ))
data.bcount <- read.csv(gsub(".xlsx", paste0("_", "fishbuyercount", ".csv"), file.econ))
data.rev <- read.csv(gsub(".xlsx", paste0("_", "Revenue", ".csv"), file.econ))
data.land <- read.csv(gsub(".xlsx", paste0("_", "Landings", ".csv"), file.econ))
data.vess <- read.csv(gsub(".xlsx", paste0("_", "VesselCharacteristics", ".csv"), file.econ))
data.days <- read.csv(gsub(".xlsx", paste0("_", "dayscrewfuelspeeddays", ".csv"),
  file.econ), stringsAsFactors = FALSE)
data.cost <- read.csv(gsub(".xlsx", paste0("_", "Costs", ".csv"), file.econ))
data.netrev <- read.csv(gsub(".xlsx", paste0("_", "NetRevenue", ".csv"), file.econ))

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
