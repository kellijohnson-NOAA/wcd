#' ----
#' Title: West coast drivers mainipulation of data before running any model
#' Author: Kelli Faye Johnson
#' Date: 2016-01-12
#' ----

#' revenue
data.rev <- data.rev[tolower(data.rev$SPGRP) == my.spp[1], ]
data.rev <- reshape(data.rev, direction = "long",
  varying = list(which(colnames(data.rev) %in% my.years)),
  v.names = "revenue",
  times = my.years, timevar = "year", drop = "SPGRP")
data.rev <- data.rev[, !colnames(data.rev) %in% "id"]
rownames(data.rev) <- NULL

#' buyer
data.bcount <- data.bcount[tolower(data.bcount$SPGRP) == my.spp[1], ]
data.bcount <- reshape(data.bcount, direction = "long",
  varying = list(which(colnames(data.bcount) %in% my.years)),
  v.names = "buyercount",
  times = my.years, timevar = "year", drop = "SPGRP")
data.bcount <- data.bcount[, !colnames(data.bcount) %in% "id"]
rownames(data.bcount) <- NULL

#' Subset the landings information for sablefish
data.md <- data.land[tolower(data.land$SPGRP) == my.spp[1], ]
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

#' bring in the other data
data.md <- merge(data.md, data.indexmatch, all = TRUE)
data.md <- merge(data.md, data.rev,    all.x = TRUE)
data.md <- merge(data.md, data.bcount, all.x = TRUE)
data.md <- merge(data.md, data.cost,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.days,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.netrev, by = good, all.x = TRUE)
data.md <- merge(data.md, data.vess,   by = good, all.x = TRUE)
data.md <- data.md[, -which(colnames(data.md) %in%
  c("Number.of.vessels.x", "Number.of.vessels.y"))]

#' add acl data
data.md <- merge(data.md, data.acl[, c("Year", "letrawl")],
  by.x = "year", by.y = "Year")
data.md$letprop <- data.md$land / (data.md$letrawl * 2204.62)

###############################################################################
###############################################################################
#' Remove NAs and standardize each column using the z transformation so that
#' the mean of the new variable is zero and the range will be roughly from
#' -3 to 3.
###############################################################################
###############################################################################
nona <- droplevels(subset(data.md, !is.na(Length) &
  portgrp != "Monterey and Morro Bay"))
nona$management <- factor(nona$management, levels = c("before", "after"))

initialcol <- grep("management", colnames(nona))
finalcol <- grep("letrawl", colnames(nona)) - 1
nona[, (initialcol + 1):finalcol] <-
  apply(nona[, (initialcol + 1):finalcol], 2,
  function(x) (x - mean(x)) / sd(x))
