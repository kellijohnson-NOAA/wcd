#' ----
#' Title: NWFSC Shelf/Slope bottom trawl survey of US West Coast groundfish
#' Author: Kelli Faye Johnson
#' Date: 2015-09-08
#' ----

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
# Set up strata
nn <- 9
strata.limits <- data.frame(
  STRATA = LETTERS[1:nn],
  NLat = c(48.5, 46.5, 45, 44, 43, 41.5, 40.5, 39, 37),
  SLat = c(46.5, 45, 44, 43, 41.5, 40.5, 39, 37, 32),
  MinDepth = rep(54.864, nn),
  MaxDepth = rep(1280, nn)
  )
nX.pos <- nX.binomial <- 1
Covariates = list(positive = TRUE, binomial = TRUE)

#+ modelstructure
modelStructures <- create_modelstructures()

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

data.svy  <- read.table(file.spu, skip = 9, header = TRUE, sep = ",")
data.svy <- data.svy[, !apply(data.svy, 2, function(x) all(is.na(x)))]
data.svy$Survey.Cycle <- sapply(strsplit(as.character(data.svy$Survey.Cycle), "Cycle "), "[", 2)
data.svy$Trawl.Start.Time <- format(as.Date(
  as.character(data.svy$Trawl.Start.Time), format = "%m/%d/%Y"), "%Y-%m-%d")
colnames(data.svy)[which(colnames(data.svy) == "Trawl.Latitude..dd.")] <- "BEST_LAT_DD"
colnames(data.svy)[which(colnames(data.svy) == "Trawl.Id")] <- "HAUL_IDENTIFIER"
colnames(data.svy)[which(colnames(data.svy) == "Survey.Cycle")] <- "YEAR"
colnames(data.svy)[which(colnames(data.svy) == "Trawl.Start.Time")] <- "Date"
colnames(data.svy)[which(colnames(data.svy) == "Trawl.Depth..m.")] <- "BEST_DEPTH_M"
data.svy <- data.svy[, !colnames(data.svy) %in%
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

data.keep <- merge(data.orig, data.svy,  all.y = TRUE,
  by = c("YEAR", "Date", "HAUL_IDENTIFIER", "BEST_DEPTH_M", "BEST_LAT_DD"))

#' Remove tows from 2013 pass 2 because they did not survey the entire area
#' less than approximately 40.5 degrees latitude due to the government shutdown.
#+ subset
remove <- with(data.keep, (YEAR == 2013 & PASS == 2 & BEST_LAT_DD < 40.5))
data.keep <- data.keep[!remove, ]
remove <- with(data.keep, is.na(PASS))
data.keep <- data.keep[!remove, ]

years <- unique(data.keep$YEAR)
strat <- strata.limits$STRATA
index <- data.frame(
  "year" = rep(years, times = length(strat)),
  "strat" = rep(strat, each = length(years)))

###############################################################################
###############################################################################
#### Step 04
#### Run individual models
###############################################################################
###############################################################################
#+ mods
oldvariables <- c(ls(), "oldvariables", "sp")

for (sp in seq_along(my.spp)) {
  # Remove all of the variables created while running a model
  rm(list = ls()[!ls() %in% oldvariables])
data.keep$HAUL_WT_KG <- data.keep[, which(colnames(data.keep) == my.spp[sp])]

chooseDat <- masterDat <- data.keep

# Set up covariates; pass_1 = (-0.5); pass_2 = (0.5)
X.bin <- X.pos <- as.matrix(chooseDat[, "PASS", drop = FALSE]) - 1.5

# Preliminary data processing
species <- my.spp[sp]
setwd(dir.results)
  dir.create(my.spp[sp], showWarnings = verbose)
  setwd(my.spp[sp])
processData()
attach(chooseDat)

mods = list()
waic <- list()
  for(it in seq_along(modelStructures)){
    message(paste("Running", my.spp[sp], "Model", it))
    mods[[it]] <- fitDeltaGLM(modelStructure = modelStructures[[it]],
      mcmc.control = mcmc.control, Parallel = parallel, Species = my.spp[sp],
      likelihood = "gamma", model.name = as.character(it),
      covariates = Covariates)
    }
  doMCMCDiags(file.path(dir.results, my.spp[sp]), mods)
  WAIC <- matrix(nrow = 2, ncol = length(modelStructures))
  for (mod in 1:length(mods)){
    Folder <- file.path(getwd(),
      paste(my.spp[sp], "FinalDiagnostics", sep = "_"), paste0("Model=", mod))
    WAIC[, mod] <- read.csv(file.path(Folder, "WAIC.csv"))$WAIC
  }

# DIC
DIC <- cbind("DIC" = sapply(mods, FUN = function(List) {List$BUGSoutput$DIC}),
  "Likelihood" = sapply(mods, FUN = function(List){List$likelihood}),
  "StrataYear" = sapply(mods, FUN = function(List){List$modelStructure$StrataYear.zeroTows}))
DIC <- cbind(DIC,
  "DeltaDIC" = as.numeric(DIC[, "DIC"]) - min(as.numeric(DIC[, "DIC"])))
DIC <- as.data.frame(DIC)
write.csv(DIC, file = file.path(dir.results, my.spp[sp], "DIC.csv"))
bestmod <- which(DIC$DeltaDIC == 0)

files2get <- list.files(pattern = "ResultsByYearAndStrata\\.csv",
  all.files = TRUE, recursive = TRUE, full.names = TRUE)
indexbyyear <- do.call("rbind", lapply(files2get, function(x) {
  results <- read.csv(x, header = TRUE)
  results$model <- substr(strsplit(x, "=")[[1]][[2]], 1, 1)
  return(results)
}))
indexspp <- subset(indexbyyear, model == bestmod)
index[match(paste(indexspp$Year, indexspp$Strata), paste(index$year, index$strat)), my.spp[sp]] <-
  indexspp$IndexMedian

detach(chooseDat)
setwd(my.dir)
}

write.csv(index, file.path(dir.results, file.index), row.names = FALSE)
index_long <- reshape(data = index, direction = "long", varying = colnames(index)[3:NCOL(index)],
  times = colnames(index)[3:NCOL(index)], timevar = "species", v.names = "index",
  new.row.names = 1:1000000)

index_long$species <- factor(index_long$species, levels = unique(index_long$species),
  labels = tolower(gsub("\\.", " ", unique(index_long$species))))
index_long$strat <- factor(index_long$strat, levels = unique(index_long$strat),
  labels = c("Washington", "Astoria", "Newport", "Coos Bay", "Brookings", "Eureka", "Fort Bragg",
             "San Francisco", "Monterey"))
png(file.path(dir.results, "index_speciesbystrata.png"),
  width = width * 2, height = height, res = resolution)
ggplot(index_long, aes(x = year, y = index, group = strat)) +
  geom_line() + geom_point() +
  facet_grid(species ~ strat, scales = "free") +
  ylab("relative index of abundance") +
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
