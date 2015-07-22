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

if("xlsx" %in% rownames(installed.packages())){
  library(xlsx)
} else {
  install.packages("xlsx", repos = 'http://cran.stat.sfu.ca/')
  library(xlsx)
  }

data.srvy <- read.xlsx("2009To2012CatchForSeminar.xlsx",
  sheetName = "CatchData2009To2012", startRow = 10, endRow = 2804,
  header = TRUE, colIndex = 1:87)
## may need to specify column classes
## argument example for xlsx::read.xlsx
## colClasses = c(rep("character", 2), rep("numeric", 3))

data.bio <- read.csv("trawl_surveys2.csv")

data.spp <- read.xlsx("econ4osu.xlsx", sheetName = "specieslist")
data.vcount <- read.xlsx("econ4osu.xlsx", sheetName = "vesseldeliverycount")
data.bcount <- read.xlsx("econ4osu.xlsx", sheetName = "fishbuyercount")
data.rev <- read.xlsx("econ4osu.xlsx", sheetName = "Revenue")
data.land <- read.xlsx("econ4osu.xlsx", sheetName = "Landings")
data.vess <- read.xlsx("econ4osu.xlsx", sheetName = "VesselCharacteristics")
data.days <- read.xlsx("econ4osu.xlsx", sheetName = "dayscrewfuelspeeddays")
data.cost <- read.xlsx("econ4osu.xlsx", sheetName = "Costs")
data.netrev <- read.xlsx("econ4osu.xlsx", sheetName = "NetRevenue")

data.hake.haul <- read.xlsx("2014-05_FinalData.xlsx", sheetName = "Hauls")
data.hake.catch <- read.xlsx("2014-05_FinalData.xlsx", sheetName = "Catch")
data.hake.bioms <- read.xlsx("2014-05_FinalData.xlsx", sheetName = "Biomass")

data.tac.after <- read.xlsx("wcd_allocation.xlsx", sheetName = "after")
data.tac.before <- read.csv("wc_tac_v3.csv")

###############################################################################
#### Save data in data dir and return to old working directory upon exit
###############################################################################
save(list = ls(pattern = "data"), file = "wcd_data_raw.RData")
setwd(dir.curr)

#endOfFile
