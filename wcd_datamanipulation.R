#' ----
#' Title: West coast drivers mainipulation of data before running any model
#' Author: Kelli Faye Johnson
#' Date: 2016-01-12
#' ----

#' read in index of abundance data
data.index <- read.csv(file.path(dir.results, file.index))
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
  lapply(3:6, function(x) {
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

#' Fix "year" column name
colnames(data.cost)[1] <- tolower(colnames(data.cost))[1]
colnames(data.days)[1] <- tolower(colnames(data.days))[1]
colnames(data.netrev)[1] <- tolower(colnames(data.netrev))[1]
colnames(data.vess)[1] <- tolower(colnames(data.vess))[1]

#' revenue
data.rev <- data.rev[tolower(data.rev$SPGRP) == my.spp[1], ]
colnames(data.rev) <- gsub("^X", "", colnames(data.rev))
data.rev <- reshape(data.rev, direction = "long",
  varying = list(which(colnames(data.rev) %in% my.years)),
  v.names = "revenue",
  times = my.years, timevar = "year", drop = "SPGRP")
data.rev <- data.rev[, !colnames(data.rev) %in% "id"]
rownames(data.rev) <- NULL

#' buyer
data.bcount <- data.bcount[tolower(data.bcount$SPGRP) == my.spp[1], ]
colnames(data.bcount) <- gsub("^X", "", colnames(data.bcount))
data.bcount <- reshape(data.bcount, direction = "long",
  varying = list(which(colnames(data.bcount) %in% my.years)),
  v.names = "buyercount",
  times = my.years, timevar = "year", drop = "SPGRP")
data.bcount <- data.bcount[, !colnames(data.bcount) %in% "id"]
rownames(data.bcount) <- NULL

#' Subset the landings information for sablefish
data.md <- data.land[tolower(data.land$SPGRP) == my.spp[1], ]
colnames(data.md) <- gsub("^X", "", colnames(data.md))
data.md <- reshape(data.md, direction = "long",
  varying = list(which(colnames(data.md) %in% my.years)),
  v.names = "land",
  times = my.years, timevar = "year", drop = "SPGRP")
data.md <- data.md[, !colnames(data.md) %in% "id"]
rownames(data.md) <- NULL

#' Add in port groups with zero landings
data.md <- merge(data.md,
  setNames(expand.grid(unique(data.md$portgrp),
  unique(data.md$year), unique(data.md$GEAR)),
  c("portgrp", "year", "GEAR")), all.y = TRUE)
data.md$land[is.na(data.md$land)] <- 0

#' Create the management variable
data.md$management <- ifelse(as.character(data.md$year) <= 2010,
  "before", "after")

#' Calculate the proportion of landings from the trawl fishery as compared
#' to fixed gear from the landings data supplied by EDC
#' Trawl is the second entry b/c alphabetically it comes after Fixed gear
data.md <- merge(data.md, data.frame(setNames(
  aggregate(land ~ year + portgrp, data = data.md, function(x) {
  x[2] / sum(x) }), c("year", "portgrp", "proportion")),
  "GEAR" = "Trawl"), all.x = TRUE)

#' bring in the other data
data.md <- merge(data.md, data.indexmatch, all = TRUE)
data.md <- merge(data.md, data.rev,    all.x = TRUE)
data.md <- merge(data.md, data.bcount, all.x = TRUE)
data.md <- merge(data.md, data.cost,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.days,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.netrev, by = good, all.x = TRUE)
data.md <- merge(data.md, data.vess,   by = good, all.x = TRUE)
data.md$Speed[grepl("^#N", data.md$Speed)|is.na(data.md$Speed)] <- -999
mode(data.md$Speed) <- "numeric"
data.md$buyercount <- as.numeric(as.character(data.md$buyercount))
data.md <- data.md[, -which(colnames(data.md) == "Number.of.vessels.y")]

#' add acl data
data.md <- merge(data.md, data.acl, by.x = "year", by.y = "Year")
data.md$letprop <- data.md$land / (data.md$letrawl * 2204.62)

###############################################################################
###############################################################################
#' Remove NAs and standardize each column using the z transformation so that
#' the mean of the new variable is zero and the range will be roughly from
#' -3 to 3.
###############################################################################
###############################################################################
nona <- droplevels(subset(data.md, !is.na(land) &
  portgrp != "Monterey and Morro Bay" &
  GEAR == "Trawl"))
nona$management <- factor(nona$management, levels = c("before", "after"))

initialcol <- grep("proportion", colnames(nona))
finalcol <- grep("OFL", colnames(nona)) - 1
nona[, (initialcol + 1):finalcol] <-
  apply(nona[, (initialcol + 1):finalcol], 2,
  function(x) (x - mean(x)) / sd(x))
