#' ----
#' Title: West coast drivers mainipulation of data before running any model
#' Author: Kelli Faye Johnson
#' Date: 2016-01-12
#' ----

#' order of port groups
portgrouporder <- c(9, 1, 7, 3, 2, 4, 5, 8, 6)

#' read in index of abundance data
data.index <- read.csv(file.path(dir.results, file.index))
data.index$strat <- factor(data.index$strat, levels(data.index$strat),
  levels(data.cost$portgrp)[portgrouporder])
colnames(data.index)[which(colnames(data.index) == "strat")] <- "portgrp"
good <- c("portgrp", "year", "GEAR")

#' Change from -999 to NA and fix "year" column name
data.cost[data.cost == -999] <- NA
data.days[data.days == -999] <- NA
colnames(data.cost)[1] <- tolower(colnames(data.cost))[1]
colnames(data.days)[1] <- tolower(colnames(data.days))[1]
colnames(data.netrev)[1] <- tolower(colnames(data.netrev))[1]

#' Subset the landings information for sablefish
data.md <- data.land[data.land$SPGRP == "Sablefish", ]
colnames(data.md) <- gsub("^X", "", colnames(data.md))

#' Switch landings information from wide to long
data.md <- reshape(data.md, direction = "long",
  varying = list(which(colnames(data.md) %in% my.years)),
  v.names = "land",
  times = my.years, timevar = "year",
  new.row.names = 1:1000000, drop = "SPGRP")[, -5]

#' switch revenue information from wide to long
data.rev <- data.rev[data.rev$SPGRP == "Sablefish", ]
colnames(data.rev) <- gsub("^X", "", colnames(data.rev))
data.rev <- reshape(data.rev, direction = "long",
  varying = list(which(colnames(data.rev) %in% my.years)),
  v.names = "revenue",
  times = my.years, timevar = "year",
  new.row.names = 1:1000000, drop = "SPGRP")[, -5]
data.rev$revenue[data.rev$revenue == -999] <- NA

#' Add in port groups with zero landings
all <- expand.grid(unique(data.md$portgrp),
  unique(data.md$year), unique(data.md$GEAR))
colnames(all) <- c("portgrp", "year", "GEAR")
data.md <- merge(data.md, all, all.y = TRUE)
rm(all)
data.md$land[is.na(data.md$land)] <- 0
data.md$land[data.md$land == -999] <- NA

#' Create the management variable
data.md$management <- ifelse(as.character(data.md$year) <= 2010,
  "before", "after")

#' Calculate the proportion of landings from the trawl fishery as compared
#' to fixed gear from the landings data supplied by EDC
temp <- aggregate(land ~ year + portgrp, data = data.md, function(x) {
  x[2] / sum(x) })
colnames(temp)[colnames(temp) == "land"] <- "proportion"
temp$GEAR <- "Trawl"
data.md <- merge(data.md, temp)
rm(temp)
data.md <- data.md[data.md$GEAR == "Trawl", ]

#' bring in the other data
data.md <- merge(data.md, data.index)
data.md <- merge(data.md, data.rev, all.x = TRUE)
data.md <- merge(data.md, data.cost,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.days,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.netrev, by = good, all.x = TRUE)
mode(data.md$Speed) <- "numeric"
data.md <- data.md[, -which(colnames(data.md) == "Number.of.vessels.y")]
data.md[data.md == -999] <- NA

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
nona <- droplevels(subset(data.md, !is.na(proportion) &
  portgrp != "Monterey and Morro Bay"))
nona$management <- factor(nona$management, levels = c("before", "after"))

initialcol <- grep("proportion", colnames(nona))
finalcol <- grep("VarCostNetRev", colnames(nona))
nona[, (initialcol + 1):finalcol] <-
  apply(nona[, (initialcol + 1):finalcol], 2,
  function(x) (x - mean(x)) / sd(x))
