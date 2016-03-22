###############################################################################
###############################################################################
## Purpose:    West coast drivers of fishermen choice
##             Read in the index data
## Author:     Kelli Faye Johnson
## Contact:    kellifayejohnson@gmail.com
## Date:       2016-02-29
## Comments:
###############################################################################
###############################################################################

#' read in index of abundance data
temp <- file.path(dir.results, file.index)
if (file.exists(temp)) {
  data.index <- read.csv(temp)
  rm(temp)
} else {
  if (verbose) message(paste(temp, "does not exist"))
  rm(temp)
}


if (!exists("strata.limits")) {
  temp <- readLines(dir(pattern = "survey", full.names = TRUE),
    n = 50)
  temp <- temp[seq(min(grep("strata", temp)), max(grep("strata", temp)))]
  eval(parse(text = temp))
}
data.area <- tail(list.files(dir.results, full.names = TRUE,
  pattern = "ByYearAndStrata\\.csv", recursive = TRUE), 1)
data.area <- read.csv(data.area)
data.area <- data.area[data.area$Year == my.years[1], c("Strata", "Area")]
data.area$SLat <- strata.limits$SLat[match(strata.limits$STRATA, data.area$Strata)]
data.area$SLat <- round(data.area$SLat, 0)
data.area <- aggregate(Area ~ SLat, data = data.area, sum)
data.area$Strata <- LETTERS[NROW(data.area):1]

#' Divide the index of abundance by the area
data.indexwoarea <- data.index
data.indexwoarea[,
  -which(colnames(data.indexwoarea) %in% c("strat", "year"))] <-
  data.indexwoarea[,
  -which(colnames(data.indexwoarea) %in% c("strat", "year"))] /
  data.area$Area[
  match(as.character(data.indexwoarea$strat), data.area$Strata)]

data.indexmatch <- do.call("rbind",
  lapply(1:length(my.years), function(x) {
  data.frame(
  "portgrp" = c("Astoria and Tillamook",
    "Brookings and Crescent City",
    "Coos Bay", "Eureka", "Fort Bragg",
    "Monterey and Morro Bay", "Newport",
    "San Francisco and Bodega Bay", "Washington"),
  "top" = c("A", "B", "A", "B", "B", "C", "A", "B", "A"),
  "bot" = c("B", "C", "B", "C", "C", "D", "B", "C", "B"),
  "year" = my.years[x])
}))
data.indexmatch <- Reduce(function(...) merge(..., all = TRUE),
  lapply(3:NCOL(data.indexwoarea), function(x) {
    a <- merge(data.indexmatch, data.indexwoarea[, c(1, 2, x)],
      by.x = c("top", "year"), by.y = c("strat", "year"))
    a <- a[, -which(colnames(a) %in% c("top", "bot"))]
    b <- merge(data.indexmatch, data.indexwoarea[, c(1:x)],
      by.x = c("bot", "year"), by.y = c("strat", "year"))
    a[, NCOL(a)] <-
      a[, which(colnames(a) == colnames(data.indexwoarea)[x])] +
      b[, which(colnames(b) == colnames(data.indexwoarea)[x])]
    return(a)
}))
