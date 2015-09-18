#' ----
#' Title: West coast drivers model
#' Author: Kelli Faye Johnson
#' Date: 2015-09-18
#' ----

long <- function(data, colname, spp = "Sablefish", change = -999) {
  if ("SPGRP" %in% colnames(data)) {
    data <- subset(data, SPGRP == spp)
  }
  vnames <- grep("X", colnames(data), value = TRUE)
  years <- gsub("X", "", vnames)
  temp <- reshape(data, direction = "long", v.names = colname,
    varying = list(vnames))
  temp$year <- factor(temp$time, levels = unique(temp$time),
    labels = years)
  temp <- temp[, -which(colnames(temp) %in% c("time", "id", "SPGRP"))]
  temp[temp[, colname] == -999, colname] <- change
  return(temp)
}

splt <- function(data, change) {
  data$GEAR <- factor(data$GEAR, levels = levels(data$GEAR), labels = c("fx", "tw"))
  colnames(data)[which(colnames(data) == "YEAR")] <- "year"
  colnames(data)[which(colnames(data) == "Fixed.costs")] <- "fixcost"
  colnames(data)[which(colnames(data) == "Variable.costs")] <- "varcost"
  colnames(data)[which(colnames(data) == "Number.of.vessels")] <- "nvess"
  all <- split(data, data$GEAR)

  all <- lapply(all, function(x) {
    temp <- x[, which(colnames(x) != "GEAR")]
    changename <- !grepl("year|portgrp", colnames(temp))
    it <- eval.parent(quote(names(X)))[substitute(x)[[3]]]
    colnames(temp)[changename] <- paste0(colnames(temp)[changename], "_", it)
    temp[temp == -999] <- change
    temp[, 3:NCOL(temp)] <- abs(temp[, 3:NCOL(temp)])
    return(temp)
  })

  return(all)
}

load(file.path(dir.data, "wcd_data_raw.Rdata"))
data.index <- read.csv(file.path(dir.results, file.index))
data.index$strat <-
factor(data.index$strat, levels(data.index$strat),
  levels(data.days$portgrp)[c(9, 1, 7, 3, 2, 4, 5, 8, 6)])
colnames(data.index)[which(colnames(data.index) == "strat")] <- "portgrp"
good <- c("portgrp", "year", "GEAR")
# Scale down the index data by 1000
docolumns <- !colnames(data.index) %in% c("year", "portgrp")
data.index[, docolumns] <- data.index[, docolumns] / 1000

colnames(data.cost)[which(colnames(data.cost) == "YEAR")] <- good[2]
data.cost[data.cost == -999] <- NA

data.cost.fx <- splt(data.cost, change = NA)[[1]]
data.cost.tw <- splt(data.cost, change = NA)[[2]]
data.days.fx <- splt(data.days, change = NA)[[1]]
data.days.tw <- splt(data.days, change = NA)[[2]]
data.netrev.fx <- splt(data.netrev, change = NA)[[1]]
data.netrev.tw <- splt(data.netrev, change = NA)[[2]]

data.md <- long(data.land, colname = "land", change = NA)
data.md$management <- ifelse(as.character(data.md$year) <= 2010, "before", "after")

#' Proportion is the decimal fraction of trawl landings to the sum of trawl and
#' (trawl + fixed gear)
data.md$proportion <- as.vector(unlist(
  tapply(data.md$land, paste(data.md$year, data.md$portgrp, sep = "."),
  FUN = function(x) {x / sum(x)})))
data.md <- subset(data.md, GEAR == "Trawl")

data.md <- merge(data.md, data.index, by = good[1:2])
# data.md <- merge(data.md,
#   subset(long(data.rev, colname = "revenue_fx", change = NA), GEAR != "Trawl",
#   select = -GEAR),
#   by = good[1:2], all.x = TRUE)
data.md <- merge(data.md, long(data.rev, colname = "revenue_tw", change = NA),
  by = good, all.x = TRUE)
data.md <- merge(data.md, data.cost.tw,   by = good[1:2], all.x = TRUE)
# data.md <- merge(data.md, data.cost.fx,   by = good[1:2], all.x = TRUE)
data.md <- merge(data.md, data.days.tw,   by = good[1:2], all.x = TRUE)
# data.md <- merge(data.md, data.days.fx,   by = good[1:2], all.x = TRUE)
data.md <- merge(data.md, data.netrev.tw, by = good[1:2], all.x = TRUE)
# data.md <- merge(data.md, data.netrev.fx, by = good[1:2], all.x = TRUE)

#' Remove data from Washington b/c they are all NA
nowa <- droplevels(subset(data.md, portgrp != "Washington" & !is.na(proportion)))

#' A model with random effects for portgrp only.
mod0 <- gamlss(proportion ~ random(portgrp),
  data = nowa, family = BEOI,
  control = gamlss.control(n.cyc = 300),
  i.control = glim.control(cc = 0.001, cyc = 50,
  bf.cyc = 400, bf.tol = 0.001)
  )

#' Find the species that matters the most
stepGAIC(mod0, scope = list(lower = ~ random(portgrp),
  upper = ~ random(portgrp) +
  darkblotched.rockfish + Pacific.ocean.perch + yelloweye.rockfish + bocaccio))

#' Find the vessel description that matters the most
stepGAIC(mod0, scope = list(lower = ~ random(portgrp),
  upper = ~ random(portgrp) + Crew_tw + Fuel_tw + Speed_tw + fixcost_tw + varcost_tw))
stepGAIC(mod0, scope = list(lower = ~ random(portgrp),
  upper = ~ random(portgrp) + Crew_tw + Fuel_tw + Speed_tw + VarCostNetRev_tw + TotCostNetRev_tw))

#' Combine best parameters in a stepwise fashion
results.final <-
  stepGAIC(mod0, scope = list(lower = ~ random(portgrp),
  upper = ~ random(portgrp) + management + Fuel_tw +  darkblotched.rockfish))
stepGAIC(mod0, scope = list(lower = ~ random(portgrp),
  upper = ~ random(portgrp) + management + TotCostNetRev_tw +  darkblotched.rockfish))

formula.final <- formula(results.final)

#' Diagnostic plots
plot(results.final)

#' Comparison of random effects using gamlss versus nlme where the
#' distribution is assumed to be normal
results.norm <- gamlss(
  proportion ~ management + random(portgrp) + darkblotched.rockfish,
  data = nowa, family = NO)
results.nlme <-
  nlme::lme(fixed = proportion ~ management + darkblotched.rockfish,
  random =  ~1 | portgrp, data = nowa, na.action = na.omit, method = "ML")
