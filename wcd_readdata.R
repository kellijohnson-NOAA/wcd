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

data.srvy <- read.xlsx("2009To2012CatchForSeminar.xlsx",
  sheetName = "CatchData2009To2012", startRow = 10, endRow = 2804,
  header = TRUE, colIndex = 1:87)
## may need to specify column classes
## argument example for xlsx::read.xlsx
## colClasses = c(rep("character", 2), rep("numeric", 3))

data.bio <- read.csv("trawl_surveys2.csv")

data.spp <- read.xlsx(file.econ, sheetName = "specieslist")
data.vcount <- read.xlsx(file.econ, sheetName = "vesseldeliverycount")
data.bcount <- read.xlsx(file.econ, sheetName = "fishbuyercount")
data.rev <- read.xlsx(file.econ, sheetName = "Revenue")
data.land <- read.xlsx(file.econ, sheetName = "Landings")
data.vess <- read.xlsx(file.econ, sheetName = "VesselCharacteristics")
data.days <- read.xlsx(file.econ, sheetName = "dayscrewfuelspeeddays")
data.cost <- read.xlsx(file.econ, sheetName = "Costs")
data.netrev <- read.xlsx(file.econ, sheetName = "NetRevenue")

data.tac.after <- read.xlsx("wcd_allocation.xlsx", sheetName = "after")
data.tac.before <- read.csv("wc_tac_v3.csv")

###############################################################################
#### Save data in data dir and return to old working directory upon exit
###############################################################################
save(list = ls(pattern = "data"), file = "wcd_data_raw.RData")
setwd(dir.curr)

#endOfFile
