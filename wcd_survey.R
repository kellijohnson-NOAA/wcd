#' ----
#' Title: NWFSC Shelf/Slope bottom trawl survey of US West Coast groundfish
#' Author: Kelli Faye Johnson
#' Date: 2015-07-21
#' ----

#+ files, echo = FALSE
# Files
file.script <- file.path("c:", "users", "kelli", "Google Drive",
  "references", "saveAllTabsAsCSV.vbs")

# Sheets
sheet.surveyspp  <- "CatchData2009To2012"
sheet.surveydata <- "HaulCatchWt&Effort"

#' The Northwest Fisheries Science Center (NWFSC) Shelf/Slope bottom trawl
#' survey data are collected to inform management of United States (U.S.)
#' West Coast groundfish fisheries. Standardized survey data is available
#' from 2003 to 2014.
#' The data, {{file.surveydata}} was provided by the NWFSC during the 2015
#' assessment cycle. Within the data, the information used was stored in the
#' {{sheet.surveydata}}

#' All output is stored in {{dir.results}}.
#' The general setup is as follows:

#+ setup
mcmc.control <- list(chains = 5, thin = 100, burnin = 5000, iterToSave = 2000)
parallel <- FALSE

#+ loadsbl, echo = FALSE
# Loading data requires using system and running a visual basic script
# that will open the excel file and save each worksheet as an individual csv
file.spp <- file.path(dir.data, file.surveyspp)
file.dat <- file.path(dir.data, file.surveydata)
file.spu <- gsub(".xlsx", paste0("_", sheet.surveyspp, ".csv"), file.spp)
file.csv <- gsub(".xlsx", paste0("_", sheet.surveydata, ".csv"), file.dat)

# Partition out the csv file if it does not already exist.
if (!file.exists(file.csv)) {
  # Must escape the forward slash to get it to work on windows
  # Fix formatting issues with Windows and R file path names and run the
  # visual basic script
  system(paste0("cscript \"", gsub("/", "\\\\", file.script),
    "\" \"", gsub("/", "\\\\", file.dat), "\\"))
}
if (!file.exists(file.spu)) {
  # Must escape the forward slash to get it to work on windows
  # Fix formatting issues with Windows and R file path names and run the
  # visual basic script
  system(paste0("cscript \"", gsub("/", "\\\\", file.script),
    "\" \"", gsub("/", "\\\\", file.spp), "\\"))
}

data.spp  <- read.table(file.spu, skip = 9, header = TRUE, sep = ",")
data.spp <- data.spp[, !apply(data.spp, 2, function(x) all(is.na(x)))]
data.spp$Survey.Cycle <- sapply(strsplit(as.character(data.spp$Survey.Cycle), "Cycle "), "[", 2)
data.spp$Trawl.Start.Time <- format(as.Date(
  as.character(data.spp$Trawl.Start.Time), format = "%m/%d/%Y"), "%Y-%m-%d")
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Latitude..dd.")] <- "BEST_LAT_DD"
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Id")] <- "HAUL_IDENTIFIER"
colnames(data.spp)[which(colnames(data.spp) == "Survey.Cycle")] <- "YEAR"
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Start.Time")] <- "Date"
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Depth..m.")] <- "BEST_DEPTH_M"
data.spp <- data.spp[, !colnames(data.spp) %in%
  c("Survey", "Position.Type", "Depth.Type", "Trawl.Longitude..dd.")]

data.orig <- read.table(file.csv, skip = 8, header = TRUE, sep = ",")
data.orig <- data.orig[, !apply(data.orig, 2, function(x) all(is.na(x)))]
data.orig$CAPTURE_DATE <- format(as.Date(data.orig$CAPTURE_DATE, format = "%m/%d/%Y"), "%Y-%m-%d")
data.orig$YEAR <- substring(data.orig$CAPTURE_DATE, 1, 4)
data.orig$AREA_SWEPT_HA <- data.orig$AREA_SWEPT_HA * 1000
colnames(data.orig)[which(colnames(data.orig) == "SURVEY_PASS")] <- "PASS"
colnames(data.orig)[which(colnames(data.orig) == "DURATION_START2END_HR")] <- "DURATION"
colnames(data.orig)[which(colnames(data.orig) == "CAPTURE_DATE")] <- "Date"
colnames(data.orig)[which(colnames(data.orig) == "AREA_SWEPT_HA")] <- "AREA_SWEPT_MSQ"
data.orig <- data.orig[, !colnames(data.orig) %in%
  c("PROJECT", "PROJECT_CYCLE", "SCIENTIFIC_NAME", "SPECIES", "HAUL_WT_KG",
  "BEST_LON_DD", "BEST_POSITION_TYPE", "BEST_DEPTH_TYPE", "AVG_WT_KG")]

masterDat <- merge(data.orig, data.spp,  all.y = TRUE,
  by = c("YEAR", "Date", "HAUL_IDENTIFIER", "BEST_DEPTH_M", "BEST_LAT_DD"))
masterDat$HAUL_WT_KG <- masterDat$sablefish

#' Remove tows from 2013 pass 2 because they did not survey the entire area
#' less than approximately 40.5 degrees latitude due to the government shutdown.
#+ subset
removed <- with(masterDat, (YEAR == 2013 & PASS == 2 & BEST_LAT_DD < 40.5))
masterDat <- masterDat[!removed, ]
removed <- with(masterDat, is.na(PASS))
chooseDat <- masterDat <- masterDat[!removed, ]

###############################################################################
###############################################################################
#### Step 04
#### Modify data slightly
###############################################################################
###############################################################################
# Get strata from table 8 of the stock assessment document
# strata is specific to species
nn <- 9
strata.limits <- data.frame(
  STRATA = LETTERS[1:nn],
  NLat = c(48.5, 46.5, 45, 44, 43, 41.5, 40.5, 39, 37),
  SLat = c(46.5, 45, 44, 43, 41.5, 40.5, 39, 37, 32),
  MinDepth = rep(54.864, nn),
  MaxDepth = rep(1280, nn)
  )

# Set up covariates
# pass_1 = (-0.5); pass_2 = (0.5)
X.bin <- X.pos <- as.matrix(chooseDat[, "PASS", drop = FALSE]) - 1.5
nX.pos <- nX.binomial <- 1
Covariates = list(positive = TRUE, binomial = TRUE)

# Preliminary data processing
setwd(dir.results)
processData()
attach(chooseDat)
on.exit(detach(chooseDat))
setwd(my.dir)

#+ modelstructure
modelStructures <- list()

# Model 1 only strata and year effects estimated, vessel-years as random
# no strata-yr interactions
modelStructures[[1]] <- list("StrataYear.positiveTows" = "zero",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "zero",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 2, fixed StrataYear interactions, vessel-years as random
# mimics J. Wallace 2011 GLMM code
modelStructures[[2]] <- list("StrataYear.positiveTows" = "fixed",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "fixed",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 3, all random interactions,
# default values from bayesGLM Writeup 2.4.pdf,
# it will take longer to run with all random effects for interactions
modelStructures[[3]] <- list("StrataYear.positiveTows" = "random",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "random",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 4, only strata and year effects, no interactions
modelStructures[[4]] <- list("StrataYear.positiveTows" = "zero",
                             "VesselYear.positiveTows" = "zero",
                             "StrataYear.zeroTows"     = "zero",
                             "VesselYear.zeroTows"     = "zero",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 5, correlated positive components for strata-year and vesselyear interactions
modelStructures[[5]] <-  list("StrataYear.positiveTows" = "correlated",
                              "VesselYear.positiveTows" = "correlated",
                              "StrataYear.zeroTows"     = "correlated",
                              "VesselYear.zeroTows"     = "correlated",
                              "Catchability.positiveTows" = "one",
                              "Catchability.zeroTows"   = "zero",
                              "year.deviations"         = "fixed",
                              "strata.deviations"       = "fixed",
                              "Vessel.positiveTows"     = "zero",
                              "Vessel.zeroTows"         = "zero")

#+ mods
years <- unique(masterDat$YEAR)
strat <- strata.limits$STRATA
index <- data.frame(
  "year" = rep(years, times = length(strat)),
  "strat" = rep(strat, each = length(years)))
mods = list()
waic <- list()

for (sp in seq_along(species)){
setwd(dir.results)
  dir.create(species[sp], showWarnings = verbose)
  setwd(species[sp])
  message(paste("Running", species[sp], "Model 1"))
  for(it in seq_along(modelStructures)){
    mods[[it]] <- fitDeltaGLM(modelStructure = modelStructures[[it]],
      mcmc.control = mcmc.control, Parallel = parallel, Species = species[sp],
      likelihood = "gamma", model.name = as.character(it),
      covariates = Covariates)
    }
  doMCMCDiags(file.path(dir.results, species[sp]), mods)
  WAIC <- matrix(nrow = 2, ncol = length(modelStructures))
  for (mod in 1:length(mods)){
    Folder <- file.path(getwd(),
      paste(species[sp], "FinalDiagnostics", sep = "_"), paste0("Model=", mod))
    WAIC[, mod] <- read.csv(file.path(Folder, "WAIC.csv"))$WAIC
  }

# DIC
DIC <- cbind("DIC" = sapply(mods, FUN = function(List) {List$BUGSoutput$DIC}),
  "Likelihood" = sapply(mods, FUN = function(List){List$likelihood}),
  "StrataYear" = sapply(mods, FUN = function(List){List$modelStructure$StrataYear.zeroTows}))
DIC <- cbind(DIC,
  "DeltaDIC" = as.numeric(DIC[, "DIC"]) - min(as.numeric(DIC[, "DIC"])))
DIC <- as.data.frame(DIC)
write.csv(DIC, file = file.path(dir.results, species[sp], "DIC.csv"))
bestmod <- which(DIC$DeltaDIC == 0)

files2get <- list.files(pattern = "ResultsByYearAndStrata\\.csv",
  all.files = TRUE, recursive = TRUE, full.names = TRUE)
indexbyyear <- do.call("rbind", lapply(files2get, function(x) {
  results <- read.csv(x, header = TRUE)
  results$model <- substr(strsplit(x, "=")[[1]][[2]], 1, 1)
  return(results)
}))
indexspp <- subset(indexbyyear, model == bestmod)
index[match(paste(indexspp$Year, indexspp$Strata), paste(index$year, index$strat)), species[sp]] <-
  indexspp$IndexMedian
}

write.csv(index, file.path(dir.results, file.index), row.names = FALSE)
