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

data.indexmatch <- do.call("rbind",
  lapply(1:length(my.years), function(x) {
  data.frame(
  "portgrp" = c("Astoria and Tillamook",
    "Brookings and Crescent City",
    "Coos Bay", "Eureka", "Fort Bragg", "Newport",
    "San Francisco and Bodega Bay", "Washington"),
  "top" = c("A", "B", "B", "C", "C", "A", "C", "A"),
  "bot" = c("B", "C", "C", "D", "D", "B", "D", "B"),
  "year" = my.years[x])
}))
data.indexmatch <- Reduce(function(...) merge(..., all = TRUE),
  lapply(3:NCOL(data.index), function(x) {
    a <- merge(data.indexmatch, data.index[, c(1, 2, x)],
      by.x = c("top", "year"), by.y = c("strat", "year"))
    a <- a[, -which(colnames(a) %in% c("top", "bot"))]
    b <- merge(data.indexmatch, data.index[, c(1:x)],
      by.x = c("bot", "year"), by.y = c("strat", "year"))
    a[, NCOL(a)] <-
      a[, which(colnames(a) == colnames(data.index)[x])] +
      b[, which(colnames(b) == colnames(data.index)[x])]
    return(a)
}))
