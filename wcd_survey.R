#' ----
#' Title: NWFSC Shelf/Slope bottom trawl survey of US West Coast groundfish
#' Author: Kelli Faye Johnson
#' Date: 2015-09-08
#' ----

#' The Northwest Fisheries Science Center (NWFSC) Shelf/Slope bottom trawl
#' survey data are collected to inform management of United States (U.S.)
#' West Coast groundfish fisheries. Standardized survey data is available
#' from 2003 to 2014.

#' All output is stored in {{dir.results}}.
#' The general setup is as follows:

#+ setup
mcmc.control <- list(chains = 5, thin = 100, burnin = 5000, iterToSave = 2000)
parallel <- FALSE

# Set up strata
# 49 is the US Canada border
tops <- c(48.5, 45, 40.5, 34.5)
bottoms <- c(tops[-1], 32.0)
#' in fathoms -> m
# depths <- c(30, 100, 300, 492.126, 700) * 1.8288
depths <- c(54.864, 183, 549, 900, 1280)

strata.limits <- data.frame(
  STRATA = LETTERS[1:(length(tops) * (length(depths) - 1))],
  NLat = rep(tops, each = length(depths) - 1),
  SLat = rep(bottoms, each = length(depths) - 1),
  MinDepth = rep(depths[-length(depths)], length(tops)),
  MaxDepth = rep(depths[-1], length(tops))
  )
strata.limits[strata.limits$STRATA == "I", "SLat"] <- 34
strata.limits[strata.limits$STRATA %in% c("A", "B"), "NLat"] <- 49
strata.limits <- strata.limits[!strata.limits$STRATA == "M", ]
strata.limits$STRATA <- LETTERS[1:NROW(strata.limits)]
rownames(strata.limits) <- 1:NROW(strata.limits)

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
indexlist <- list()

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
  if (my.spp[sp] != "sablefish") next

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
indexspp$Strata <- strata.limits$STRATA.x[ls()
  match(indexspp$Strata, strata.limits$STRATA.y)]
indexlist[[length(indexlist) + 1]] <- indexspp
index <- merge(index,
  indexspp[, c("Year", "Strata", "IndexMedian")],
  by.x = c("year", "strat"), by.y = c("Year", "Strata"),
  all.x = TRUE, all.y = TRUE)
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
colnames(temp) <- colnames(index)
write.csv(temp, file.path(dir.results, file.index), row.names = FALSE)
rm(temp)

#' Plot index data by strata and then without depth but still by strata
index_long <- reshape(data = index, direction = "long",
  varying = colnames(index)[3:NCOL(index)],
  times = colnames(index)[3:NCOL(index)], timevar = "species",
  v.names = "index")
index_long$species <- factor(index_long$species, levels = unique(index_long$species),
  labels = tolower(gsub("\\.", " ", unique(index_long$species))))

png(file.path(dir.results, "index_speciesbystrata_nodepth.png"),
  width = width, height = width, res = resolution)
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

#plot index by stratam with confidence intervals
png(file.path(dir.results, "index_speciesbystrata_ci.png"),
  width = width, height = width, res = resolution)

temp <- lapply(1:length(indexlist), function(x) {
  data.frame(indexlist[[x]][, c("Year", "Strata", "Raw", "IndexMedian", "SdLog")],
  "species" = colnames(index)[x + 2],
  "lower" = exp(log(indexlist[[x]]$IndexMedian) - 1.96 * indexlist[[x]]$SdLog),
  "upper" = exp(log(indexlist[[x]]$IndexMedian) + 1.96 * indexlist[[x]]$SdLog))
  })
temp <- do.call("rbind", temp)
temp$Strata <- factor(temp$Strata)
levels(temp$Strata) <- rep(LETTERS[1:length(unique(strata.limits$NLat))],
  each = length(unique(strata.limits$MaxDepth)))
temp <- aggregate(
  as.matrix(temp[, c("Raw", "IndexMedian", "SdLog", "lower", "upper")]) ~
  Year + species + Strata, data = temp, sum, na.rm = TRUE)
temp$species <- as.character(temp$species)
temp$species[temp$species == "Pacific.ocean.perch"] <- "POP"
temp$species[temp$species == "darkblotched.rockfish"] <- "darkblotched"

ggplot(temp) +
  geom_point(aes(x = Year, y = Raw, group = Strata)) +
  geom_line(aes(x = Year, y = IndexMedian, group = Strata), lty = 1) +
  geom_line(aes(x = Year, y = upper, group = Strata), lty = 2) +
  geom_line(aes(x = Year, y = lower, group = Strata), lty = 2) +
  facet_grid(species ~ Strata, scales = "fixed") +
  ylab("relative index of abundance") +
  xlab("year") +
  scale_x_continuous(breaks = unique(temp$Year)) +
  # xlim(c(2009, 2014)) +
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90,
      size = 8, face = "plain"))
dev.off()
rm(temp)

#EndOfFile
