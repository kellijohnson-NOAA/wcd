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
# 49 is the US Canada border
tops <- c(49.0, 47.5, 43, 40.5)
bottoms <- c(tops[-1], 34.5)
#' in fathoms -> m
depths <- c(30, 100, 300, 700) * 1.8288

strata.limits <- data.frame(
  STRATA = LETTERS[1:(length(tops) * (length(depths) - 1))],
  NLat = rep(tops, each = length(depths) - 1),
  SLat = rep(bottoms, each = length(depths) - 1),
  MinDepth = rep(depths[-length(depths)], length(tops)),
  MaxDepth = rep(depths[-1], length(tops))
  )

nX.pos <- nX.binomial <- 1
Covariates = list(positive = TRUE, binomial = TRUE)

#+ modelstructure
modelStructures <- create_modelstructures()

data.svy  <- data.srvy
data.svy$DURATION <- data.svy$DURATION_START2END_HR
data.svy$AREA_SWEPT_MSQ <- data.svy$AREA_SWEPT_HA * 10000
data.svy$PROJECT_CYCLE <- sapply(strsplit(as.character(data.svy$PROJECT_CYCLE), "Cycle "), "[", 2)
data.svy$Date <- format(as.Date(
  as.character(data.svy$CAPTURE_DATE), format = "%m/%d/%Y"), "%Y-%m-%d")
colnames(data.svy)[which(colnames(data.svy) == "PROJECT_CYCLE")] <- "YEAR"
colnames(data.svy)[which(colnames(data.svy) == "SURVEY_PASS")] <- "PASS"

data.keep <- data.svy

#' Remove tows from 2013 pass 2 because they did not survey the entire area
#' less than approximately 40.5 degrees latitude due to the government shutdown.
#+ subset
remove <- with(data.keep, (YEAR == 2013 &
  PASS == 2 & BEST_LAT_DD < 40.5))
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
strata.all <- strata.limits
oldvariables <- c(ls(), "oldvariables", "sp")

for (sp in seq_along(my.spp)) {
  if (my.spp[sp] == "yelloweye.rockfish") next
  if (my.spp[sp] == "bocaccio") next

  # Check to see if there are greater than 10 observations in the deepest depth
  if (sum(data.srvy[data.srvy[, my.spp[sp]] > 0, "BEST_DEPTH_M"] >
      tail(depths, 2)[1]) < 10) {
    strata.limits <-
      strata.limits[strata.limits$MaxDepth != max(strata.limits$MaxDepth), ]
      strata.limits$STRATA <- LETTERS[1:NROW(strata.limits)]
  }

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

strata.limits <- merge(strata.all, strata.limits,
  by = colnames(strata.limits)[-1], all.x = TRUE)

files2get <- list.files(pattern = "ResultsByYearAndStrata\\.csv",
  all.files = TRUE, recursive = TRUE, full.names = TRUE)
indexbyyear <- do.call("rbind", lapply(files2get, function(x) {
  results <- read.csv(x, header = TRUE)
  results$model <- substr(strsplit(x, "=")[[1]][[2]], 1, 1)
  return(results)
}))
indexspp <- subset(indexbyyear, model == bestmod)
head(strata.limits)
indexspp$Strata <- strata.limits$STRATA.x[
  match(indexspp$Strata, strata.limits$STRATA.y)]
index <- merge(index[, c("year", "strat")],
  indexspp[, c("Year", "Strata", "IndexMedian")])
colnames(index)[NCOL(index)] <- my.spp[sp]

detach(chooseDat)
setwd(my.dir)
strata.limits <- strata.all
}

#' Write the index data to the disk
write.csv(index, file.path(dir.results,
  gsub("\\.csv", "_withdepth\\.csv", file.index)), row.names = FALSE)

temp <- index
levels(temp$strat) <- rep(LETTERS[1:length(unique(strata.limits$NLat))],
  each = length(unique(strata.limits$MaxDepth)))
temp <- aggregate(as.matrix(cbind(temp[, -c(1:2)])) ~ year + strat,
  data = temp, sum)
write.csv(temp, file.path(dir.results, file.index), row.names = FALSE)

#' Plot index data by strata and then without depth but still by strata
index_long <- reshape(data = index, direction = "long", varying = colnames(index)[3:NCOL(index)],
  times = colnames(index)[3:NCOL(index)], timevar = "species",
  v.names = "index")
index_long$species <- factor(index_long$species, levels = unique(index_long$species),
  labels = tolower(gsub("\\.", " ", unique(index_long$species))))

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

png(file.path(dir.results, "index_speciesbystrata_nodepth.png"),
  width = width, height = height, res = resolution)
temp <- index_long
levels(temp$strat) <- rep(LETTERS[1:length(unique(strata.limits$NLat))],
  each = length(unique(strata.limits$MaxDepth)))
temp <- aggregate(index ~ year + species + strat, data = temp, sum)

ggplot(temp, aes(x = year, y = index, group = strat)) +
  geom_line() + geom_point() +
  facet_grid(species ~ strat, scales = "fixed") +
  ylab("relative index of abundance") +
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90,
      size = 8, face = "plain"))
dev.off()
rm(temp)
