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
species <- "sablefish"
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

data.orig <- read.table(file.csv, skip = 8, header = TRUE, sep = ",")
data.orig <- data.orig[, !apply(data.orig, 2, function(x) all(is.na(x)))]

data.spp$YEAR <- sapply(strsplit(as.character(data.spp$Survey.Cycle), "Cycle "), "[", 2)
data.spp$Date <- format(as.Date(
  as.character(data.spp$Trawl.Start.Time), format = "%m/%d/%Y"), "%Y-%m-%d")
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Latitude..dd.")] <- "BEST_LAT_DD"
colnames(data.spp)[which(colnames(data.spp) == "Trawl.Id")] <- "HAUL_IDENTIFIER"

data.orig$Date <- format(as.Date(data.orig$CAPTURE_DATE, format = "%m/%d/%Y"), "%Y-%m-%d")
data.orig$YEAR <- substring(data.orig$Date, 1, 4)

data.master <- merge(data.orig, data.spp, by = c("Date", "HAUL_IDENTIFIER"), all.y = TRUE)

masterDat <- data.orig


masterDat$sablefish <- masterDat$HAUL_WT_KG
masterDat$AREA_SWEPT_MSQ <- masterDat$AREA_SWEPT_HA * 10000
colnames(masterDat)[which(colnames(masterDat) == "SURVEY_PASS")] <- "PASS"
colnames(masterDat)[which(colnames(masterDat) == "DURATION_START2END_HR")] <- "DURATION"

#' Remove all information prior to 2003, which used simimlar but different methods
#' and covered a different spatial extent.
#' Also remove tows from 2013 pass 2 because they did not survey the entire area
#' less than approximately 40.5 degrees latitude due to the government shutdown.
#+ subset
masterDat <- subset(masterDat, YEAR > 2002,
  select = c("VESSEL", "YEAR", "BEST_LAT_DD", "BEST_DEPTH_M", "DURATION",
  species, "AREA_SWEPT_HA", "AREA_SWEPT_MSQ", "HAUL_IDENTIFIER", "Date", "PASS"))
subDat <- masterDat[-which(masterDat$YEAR == 2013 & masterDat$PASS == 2 &
  masterDat$BEST_LAT_DD < 40.5), ]
masterDat <- chooseDat <- subDat

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
processData()
attach(chooseDat)
on.exit(detach(chooseDat))

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
mods = list()
# Run the models and place them in the list titled "mods"
setwd(dir.results)
dir.create(species)
setwd(species)
  for(it in seq_along(modelStructures)){
    mods[[it]] <- fitDeltaGLM(modelStructure = modelStructures[[it]],
      mcmc.control = mcmc.control, Parallel = parallel, Species = species,
      likelihood = "gamma", model.name = as.character(it),
      covariates = Covariates)
    }

###############################################################################
###############################################################################
#### Step 06
#### Run diagnostics, which are automatically placed in a folder
###############################################################################
###############################################################################
# # Process MCMC output
# # Make sure that Data is attached prior to running
doMCMCDiags(dir.results, mods)
# lapply(mods, logDensity)
# # Get the WAIC values
WAIC <- matrix(nrow = 2, ncol = length(mods))
for (mod in 1:length(mods)){
  Folder <- file.path(getwd(),
    paste(species, "FinalDiagnostics", sep = "_"), paste0("Model=", mod))
  WAIC[, mod] <- read.csv(file.path(Folder, "WAIC.csv"))$WAIC
}

# DIC
DIC <- cbind("DIC" = sapply(mods, FUN = function(List) {List$BUGSoutput$DIC}),
  "Likelihood" = sapply(mods, FUN = function(List){List$likelihood}),
  "StrataYear" = sapply(mods, FUN = function(List){List$modelStructure$StrataYear.zeroTows}))
DIC <- cbind(DIC,
  "DeltaDIC" = as.numeric(DIC[, "DIC"]) - min(as.numeric(DIC[, "DIC"])))
write.csv(DIC, file = file.path(dir.results, "DIC_table.csv"))
DIC_table <- read.csv(file.path(dir.results, "DIC_table.csv"), header = TRUE)

if (check_prev == TRUE){
  ### through 2010 only
  reconstruction_file <- dir(file.path(dir.results,
    paste(species, "FinalDiagnostics", sep = "_"), "Model=1"),
    pattern = "ResultsByYear.csv", full.names = TRUE)
  res_reconstruct <- read.csv(reconstruction_file, header = TRUE)
  res_reconstruct$Raw <- res_reconstruct$Raw / 1000
  res_reconstruct$IndexMedian <- res_reconstruct$IndexMedian / 1000
  res_reconstruct$IndexMean <- res_reconstruct$IndexMean / 1000
  write.csv(res_reconstruct,
    file.path(dir.results, "bestmodel_index_reconstruct.csv"))
}
detach(chooseDat)
setwd(dir.check)
} # End of loop for check_prev

###############################################################################
###############################################################################
#### Step 07
#### Create a plot with the results compared to the previous assessment
###############################################################################
###############################################################################
### Figure 12
files2get <- sapply(1:length(modelStructures), function(x) {
  dir(file.path(dir.data, paste(species, "FinalDiagnostics", sep = "_"),
  paste0("Model=", x)), pattern = "ResultsByYear.csv", full.names = TRUE)
})
indexbyyear <- do.call("rbind", lapply(files2get, function(x) {
  results <- read.csv(x, header = TRUE)
  results$model <- substr(strsplit(x, "=")[[1]][[2]], 1, 1)
  return(results)
}))

# Pull out summary information from 2011 assessment results wrt GLMM results
# for the NWFSC survey trawl: "NWFSC combo GLMM Good"
file.oldindex <- file.path(dir.data.prev, "Survey series.xlsx")
file.oldindexcsv <- gsub(".xlsx", "_Sheet1.csv", file.oldindex)
if (!file.exists(file.oldindexcsv)) {
  system(paste0("cscript \"",
    gsub("/", "\\", file.script, fixed = TRUE), "\" \"",
    gsub("/", "\\", file.oldindex, fixed = TRUE), "\\"))
}
if (!file.exists(file.oldindexcsv)) {
  stop(paste("Data file,", file.oldindexcsv, "does not exist.",
             "I am guessing the visual basic script did not run.",
             "Check that", file.script, "exists"))
}
oldindex <- read.table(file.oldindexcsv, skip = 42, sep = ",",
  stringsAsFactors = FALSE)[1:9, 1:10]
colnames(oldindex) <- as.character(oldindex[1, ])
oldindex <- oldindex[-1, ]
oldindex <- as.data.frame(apply(oldindex, 2,
  function(x) as.numeric(gsub(",", "", x))))

## Sum the absolute value of the differences between the median MT of the new
## model with the chosen GLMM from 2011.
## variance used the differences between the standard deviation of the log MT
indexbyyear_match <- indexbyyear[-which(indexbyyear$Year > 2010),]
indexdiffs <- do.call("rbind", tapply(seq(dim(indexbyyear_match)[1]),
  indexbyyear_match$model, function(x) {
  mtdiff <- indexbyyear_match$IndexMedian[x] / 1000 - oldindex$"Mean centered"
  cvdiff <- indexbyyear_match$SdLog[x] - oldindex$SD.of.log.MT
  return(cbind(sum(abs(mtdiff)), sum(abs(cvdiff))))
}))
colnames(indexdiffs) <- c("Median.MT", "sdLog")
similar_model <- which(indexdiffs[, 1] == min(indexdiffs[, 1]))
DIC_model <- which(DIC_table$DeltaDIC == min(DIC_table$DeltaDIC))
best_model <- DIC_model
# mods[[bestmodel]]$modelStructure

# Generate plot "modelComparison"
png(file.path(dir.data, "modelComparison.png"))
par(mar = c(6, 5, 4, 4))
with(subset(indexbyyear, model == best_model), plot(Year, IndexMedian / 1000,
  las = 1, ylab = "", main = "", pch = 19, col = "blue", type = "o",
  ylim = c(min(c(IndexMedian / 1000, oldindex$Median.MT)),
           max(c(IndexMedian / 1000, oldindex$Median.MT))),
  xlab = "Year"))
# with(subset(indexbyyear, model == best_model), lines(Year, IndexMedian/1000,
#      las = 1, ylab = "", main = "", pch = 19, col="forestgreen", type="o",
#      ylim = c(min(c(IndexMedian/1000, oldindex$Median.MT)),
#               max(c(IndexMedian/1000, oldindex$Median.MT))),
#      xlab = "Year"))
# mtext(side = 1, text = "Differences between estimates from the GLMM used in 2011 and tested GLMMs for 2015",
#       line = 4)
mtext(side = 2, text = "Median metric tons", line = 4)
with(oldindex, points(YEAR, Median.MT, pch = 15, col="red", type="o"))
for(i in (1:5)[-best_model]){
	sub <- indexbyyear[which(indexbyyear[,"model"]==i),]
	points(sub$Year, sub$IndexMedian/1000, pch=i)
}
legend("topright", legend = c("2011 GLMM", "Best GLMM 2015"),
  pch = c(15, 19), bty = "n", col = c("red", "blue"))
text(x = 2008, y = 125000, col = "blue", paste(names(modelStructures[[3]]),
  collapse = "\n"))
text(x = 2012, y = 125000, col = "blue",
  paste(as.vector(unlist(modelStructures[[3]])), collapse = "\n"))
dev.off()

###############################################################################
###############################################################################
#### Step
#### Create plot of this years model, best model, and last model
###############################################################################
###############################################################################
png(file.path(dir.data, "modelComparison_35old.png"), res = 300,
  height = 6, width = 8, units = "in")
par(mgp = c(3, 0.3, 0), oma = rep(0, 4), las = 0, mar = c(2, 4, 1, 1))
with(subset(indexbyyear, model == best_model), plot(Year, IndexMedian / 1000,
  ylab = "", main = "", pch = 19, col = "blue", type = "o",
  ylim = c(min(c(IndexMedian / 1000, oldindex$Median.MT)),
           max(c(IndexMedian / 1000, oldindex$Median.MT))),
  xlab = ""))
with(subset(indexbyyear, model == 3), points(Year, IndexMedian / 1000,
  pch = 19, col = "black", type = "o"))
with(oldindex, points(YEAR, Median.MT, pch = 15, col = "red", type="o"))
legend("topright", legend = c("2011 delta-GLMM", "2015 delta-GLMM DIC",
  "2015 delta-GLMM used"), pch = c(15, 19, 19), col = c("red", "blue", "black"),
  lty = 1, bty = "n")
text(x = 2008, y = 135000, col = "black", paste(c("-2015 model structure-\n",
  names(modelStructures[[3]])), collapse = "\n"))
mtext("Median metric tons", side=2, line=2)
dev.off()

png(file.path(dir.data, "modelComparison_old.png"), res = 300,
  height = 6, width = 8, units = "in")
par(mgp = c(3, 0.3, 0), oma = rep(0, 4), las = 0, mar = c(2, 4, 1, 1))
with(subset(indexbyyear, model == 3), plot(Year, IndexMedian / 1000,
  ylab = "", main = "", pch = 19, col = "black", type = "o",
  ylim = c(min(c(IndexMedian / 1000, oldindex$Median.MT)),
           max(c(IndexMedian / 1000, oldindex$Median.MT))),
  xlab = ""))
with(oldindex, points(YEAR, Median.MT, pch = 15, col = "red", type="o"))
legend("topright", legend = c("2011 delta-GLMM", "2015 delta-GLMM"),
  pch = c(15, 19, 19), col = c("red", "black"),
  lty = 1, bty = "n")
text(x = 2008, y = 135000, col = "black", paste(c("-2015 model structure-\n",
  names(modelStructures[[3]])), collapse = "\n"))
mtext("Median metric tons", side=2, line=2)
dev.off()

###############################################################################
###############################################################################
#### Step 08
#### plots that match assessment
###############################################################################
###############################################################################
### Figure 9
length.csv <- gsub(".xlsx", "_Lengths.csv", file.dat)
data.lengths <- read.table(length.csv, skip = 8, header = TRUE, sep = ",")
maxdepth <- strata.limits[1:3, "MaxDepth"]
slat <- unique(strata.limits$SLat)[c(1, 2, 4)]
png(file.path(dir.data, "lengths_depth.png"), width = 620, height = 480)
par(mar = c(6, 5, 4, 4))
with(data.lengths, plot(x = DEPTH_M, y = LENGTH_CM, pch = 16, col = "#80808070",
  xlab = "Depth (m)", ylab = "Length (cm)"))
loess_depth <- with(data.lengths,
  loess.smooth(x = DEPTH_M, y = LENGTH_CM, family = "gaussian"))
lines(x = loess_depth$x, y = loess_depth$y, lwd = 4, col = "green")
invisible(sapply(maxdepth,
  function(x) abline(v = x, col = "darkblue", lwd = 3)))
dev.off()

### Figure 10
png(file.path(dir.data, "lengths_lat.png"), width = 620, height = 480)
par(mar = c(6, 5, 4, 4))
with(data.lengths, plot(x = HAUL_LATITUDE_DD, y = LENGTH_CM, pch = 16,
  col = "#80808070", xlab = "Latitude", ylab = "Length (cm)"))
loess_lat <- with(data.lengths,
  loess.smooth(x = HAUL_LATITUDE_DD, y = LENGTH_CM, family = "gaussian"))
lines(x = loess_lat$x, y = loess_lat$y, lwd = 4, col = "green")
invisible(sapply(slat,
  function(x) abline(v = x, col = "darkblue", lwd = 3)))
dev.off()

### Figure 11
files2get <- sapply(1:length(modelStructures), function(x) {
  dir(file.path(dir.data, paste(species, "FinalDiagnostics",
  sep = "_"), paste0("Model=", x)), pattern = "ResultsByYear.csv",
  full.names = TRUE)
})
indexbyyear <- do.call("rbind", lapply(files2get, function(x) {
  results <- read.csv(x, header = TRUE)
  results$model <- substr(strsplit(x, "=")[[1]][[2]], 1, 1)
  return(results)
}))
index_adjusted <- indexbyyear
index_adjusted$IndexMedian <- indexbyyear$IndexMedian / 1000
index_adjusted$IndexMean <- indexbyyear$IndexMean / 1000
index_adjusted$Raw <- indexbyyear$Raw / 1000
write.csv(index_adjusted, file.path(dir.results, "indexbyyear_adjusted.csv"))
### best model == 3
### WAIC, and compared to previous assessment (see Section 7 above)
### DIC chooses model 5
### USE SdLog
### CONVERT TO CORRECT UNITS
### Also look at Results by Year and Strata, not just ResultsByYear
res <- subset(index_adjusted, model == 3)
write.csv(res, file.path(dir.results, "bestmodel_index.csv"))
res$sd <- res$CvMedian*res$IndexMedian
res$lci <- qnorm(0.025, res$IndexMedian, res$sd)
res$lci2 <- qnorm(0.25, res$IndexMedian, res$sd)
res$uci <- qnorm(0.975, res$IndexMedian, res$sd)
res$uci2 <- qnorm(0.75, res$IndexMedian, res$sd)
yaxis <- c(0, max(res$uci)*1.15)
ylab <- c("0", "50,000", "100,000", "150,000", "200,000")
png(file.path(dir.data, "bestModel2015.png"))
par(mfrow = c(1, 1), mar = c(4, 5, 2, 2), omi = c(0.4, 0.4, 0, 0))
plot(x = 1, y = 1, type = "n",
  xlim = c(res$Year[1] - 1, res$Year[nrow(res)] + 1), ylim = yaxis,
 xaxs = "i", yaxs = "i", xlab = "", ylab = "", yaxt = "n", font.lab = 2)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000), labels = ylab,
  las = 2)
polygon(x = c(res$Year[1]:res$Year[nrow(res)], res$Year[nrow(res)]:res$Year[1]),
  y = c(res$lci, rev(res$uci)), col = "#88888850", border = NA)
polygon(x = c(res$Year[1]:res$Year[nrow(res)], res$Year[nrow(res)]:res$Year[1]),
  y = c(res$lci2, rev(res$uci2)), col = "#888888AA", border = NA)
lines(x = res$Year, y = res$IndexMedian, lwd = 2, type = "o", pch = 19,
  cex = 1.5)
my.colors <- c("black", "#88888850", "#888888AA")
legend("topright", col = my.colors, pch = 15,
  legend = c("Median", "95% CI", "50% CI"), cex = 1.3, bty = "n")
mtext(side = 2, "Survey Index", line = 5, font = 2, cex = 1.5)
mtext(side = 1, "Year", line = 3, font = 2, cex = 1.5)
dev.off()

###############################################################################
###############################################################################
#### Step 09
#### upon exit set the directory to the old working directory
###############################################################################
###############################################################################
setwd(dir.check)
