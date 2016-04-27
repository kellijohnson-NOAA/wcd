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

###############################################################################
###############################################################################
#' Create the binomial data
###############################################################################
###############################################################################
data.binom <- data.md
# todo: change to using the actual number of days, need to get this from ES.
data.binom$fullday <- ceiling(with(data.binom, Days * Number.of.vessels))
data.binom <- merge(data.binom,
  aggregate(fullday ~ year + portgrp, data = data.binom, sum),
  by = c("year", "portgrp"))
data.binom <- data.binom[!is.na(data.binom$fullday.x), ]
data.binom <- data.binom[data.binom$GEAR == "Fixed gear", ]
data.binom$y <- with(data.binom, cbind(fullday.x, fullday.y - fullday.x))

###############################################################################
###############################################################################
#' Create a subset with only port groups that have both trawl and fixed gear
#' information in a given year.
#' Results in a very small data set.
###############################################################################
###############################################################################

data.match <- data.md[!is.na(data.md$Fixed.costs), ]
data.match$name <- with(data.match, paste0(year, portgrp))
data.match <- subset(data.match, data.match$name %in% data.match$name[data.match$GEAR == "Fixed gear"])
data.match <- subset(data.match, data.match$name %in% data.match$name[data.match$GEAR == "Trawl"])
data.match <- data.match[!is.na(data.match$land), ]

for(ii in data.match$name[data.match$GEAR == "Fixed gear"]) {
    temp <- data.match[data.match$name == ii, ]
    data.match[data.match$name == ii, "proportion"] <- temp$land / sum(temp$land)
}
data.match.trawl <- subset(data.match, GEAR == "Trawl")
data.match.fixed <- subset(data.match, GEAR == "Fixed gear")
data.match.fixed <- data.match.fixed[, -which(colnames(data.match.fixed) == "Speed")]

###############################################################################
###############################################################################
#' Remove NAs and standardize each column using the z transformation so that
#' the mean of the new variable is zero and the range will be roughly from
#' -3 to 3.
###############################################################################
###############################################################################
data.match.fixed[, my.columns] <- apply(data.match.fixed[, my.columns], 2,
  function(x) (x - mean(x)) / sd(x))
data.binom[, my.columns] <- apply(data.binom[, my.columns], 2,
  function(x) (x - mean(x)) / sd(x))

# EndOfFile
