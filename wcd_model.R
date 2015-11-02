#' ----
#' Title: West coast drivers model
#' Author: Kelli Faye Johnson
#' Date: 2015-09-18
#' ----

# order of port groups
portgrouporder <- c(9, 1, 7, 3, 2, 4, 5, 8, 6)
data.index <- read.csv(file.path(dir.results, file.index))
data.index$strat <- factor(data.index$strat, levels(data.index$strat),
  levels(data.cost$portgrp)[portgrouporder])
colnames(data.index)[which(colnames(data.index) == "strat")] <- "portgrp"
good <- c("portgrp", "year", "GEAR")
# Scale down the index data by 1000
docolumns <- !colnames(data.index) %in% c("year", "portgrp")
data.index[, docolumns] <- data.index[, docolumns] / 1000000

colnames(data.cost)[which(colnames(data.cost) == "YEAR")] <- "year"
data.cost <- split(data.cost, data.cost$GEAR)$Trawl
days.days <- split(data.days, data.days$GEAR)$Trawl
data.cost[data.cost == -999] <- NA
data.days[data.days == -999] <- NA
colnames(data.days)[1] <- tolower(colnames(data.days))[1]
colnames(data.netrev)[1] <- tolower(colnames(data.netrev))[1]

###############################################################################
#' Calculate the proportion of landings from the trawl fishery as compared
#' to fixed gear from the landings data supplied by EDC
data.md <- subset(data.land, SPGRP == "Sablefish")
data.md <- data.md[, -which(colnames(data.md) == "SPGRP")]
data.md <- reshape(data.md, direction = "long",
  varying = grep("X", colnames(data.md)), v.names = "land",
  times = gsub("X", "", grep("X", colnames(data.md), value = TRUE)))
colnames(data.md)[colnames(data.md) == "time"] <- "year"
mode(data.md$year) <- "numeric"
data.md <- data.md[, -which(colnames(data.md) == "id")]
data.md$land[data.md$land == -999] <- NA
data.md$management <- ifelse(as.character(data.md$year) <= 2010, "before", "after")
rownames(data.md) <- NULL
#' Proportion
data.md$proportion <- NA
for(yr in unique(data.md$year)) {
  subs <- subset(data.md, year == yr)
  # Make sure trawl and fixed gear exist
  for (pg in levels(subs$portgrp)) {
    temp <- subset(subs, portgrp == pg)
    if (NROW(temp) == 1) {
      use <- ifelse(temp$GEAR == "Trawl", "Fixed gear", "Trawl")
      subs <- rbind(subs, c(pg, use, temp$year, 0, temp$management, NA))
    }
  }
  mode(subs$land) <- "numeric"
  sums <- tapply(subs$land, subs[, c("portgrp", "GEAR")], sum)
  sums <- t(apply(sums, 1, function(x) x/sum(x)))
  matches <- match(paste0(yr, rownames(sums), "Trawl"),
    apply(data.md[, c("year", "portgrp", "GEAR")], 1, paste, collapse = ""))
  data.md[matches, "proportion"] <- sums[, "Trawl"]
}
data.md <- subset(data.md, GEAR == "Trawl")

data.md <- cbind(data.md, data.index[match(paste0(data.md$portgrp, data.md$year),
  paste0(data.index$portgrp, data.index$year)), -c(1:2)])

data.md <- merge(data.md, long(data.rev, colname = "revenue", change = NA),
  by = good, all.x = TRUE)
data.md <- merge(data.md, data.cost,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.days,   by = good, all.x = TRUE)
data.md <- merge(data.md, data.netrev, by = good, all.x = TRUE)
mode(data.md$Speed) <- "numeric"

###############################################################################
###############################################################################
#' Remove NAs and standardize each column using the z transformation so that
#' the mean of the new variable is zero and the range will be roughly from
#' -3 to 3.
###############################################################################
###############################################################################
nona <- droplevels(subset(data.md, !is.na(proportion)))
nona$management <- factor(nona$management, levels = c("before", "after"))
####
#' report results before z scoring
econstuff <- aggregate(
  cbind(Fixed.costs, Variable.costs, Crew, Fuel, Speed) ~ portgrp,
  data = data.md, mean)
econstuff <- cbind(aggregate(proportion ~ portgrp, nona, length),
  econstuff[, -1])
econstuff <- econstuff[portgrouporder, ]
colnames(econstuff) <- gsub("proportion", "n", colnames(econstuff))
colnames(econstuff) <- gsub("\\.", " ", colnames(econstuff))
colnames(econstuff) <- gsub("grp", " group", colnames(econstuff))

sink(file.path(dir.results, "econ.tex"))
print(xtable(econstuff, digits = 2), include.rownames = FALSE)
sink()
system(paste("pandoc", file.path(dir.results, "econ.tex"), "-o", file.path(dir.results, "econ.docx")))
####
initialcol <- grep("proportion", colnames(nona))
nona[, (initialcol + 1):NCOL(nona)] <-
  apply(nona[, -c(1:initialcol)], 2, function(x) (x - mean(x)) / sd(x))

###############################################################################
###############################################################################
#' All covariates:
#' management +
#' yelloweye.rockfish + darkblotched.rockfish + Pacific.ocean.perch + bocaccio
#' Crew + Fuel + Variable.costs + Fixed.costs + Speed
###############################################################################
###############################################################################
control <- gamlss.control(n.cyc = 300)
i.control <- glim.control(cc = 0.001, cyc = 50,
  bf.cyc = 400, bf.tol = 0.001)
covariates <- c("yelloweye.rockfish", "darkblotched.rockfish",
  "Pacific.ocean.perch", "bocaccio", "Crew", "Fuel", "Variable.costs",
  "Fixed.costs", "Speed")

all <- lapply(sapply(
  unlist(lapply(seq(covariates),
    function(i) combn(seq(covariates), i, simplify = FALSE)),
      recursive = FALSE),
    function(ii) paste("proportion ~ management +",
      paste0(covariates[ii], collapse = " + "))
  ),
  function(iii) gamlss(as.formula(iii),
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  ))
summary(all[[which.min(lapply(all, AICc))]])

par(mfrow = c(3, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 1, 1))
for (i in seq(covariates)) {
  get <- strsplit(covariates[i], "\\.")[[1]][1]
  temp <- sapply(all, "[", "mu.coefficients")[
    grep(get, sapply(sapply(all, "[", "mu.coefficients"), names))]
plot(sapply(all[grep(get, sapply(sapply(all, "[", "mu.coefficients"), names))],
  AICc), sapply(temp, function(x) {
  nm <- names(x)
  use <- grep(paste0(get, "|management|Inter"), nm)
  invlogit(sum(x[use])) - invlogit(sum(x[grep("management|Inter", nm)]))}),
  xlab = "", ylab = "", ylim = c(-1, 1),
  xlim = c(min(sapply(all, AICc)), median(sapply(all, AICc))),
  xaxt = ifelse(i %in% 7:9, "s", "n"),
  yaxt = ifelse(i %in% c(1, 4, 7), "s", "n"))
  legend("topright", legend = gsub("\\.", " ", covariates[i]), bty = "n")
  abline(h = 0, col = "red", lty = 2)
  if(i == 1) mtext(side = 2, outer = TRUE,
    text = "change in proportion with a one unit increase", line = 2)
  if(i == 1) mtext(side = 1, outer = TRUE, text = "AICc", line = 2)
}

#' A model with random effects for portgrp only.
mod0 <- gamlss(proportion ~ 1,
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  )

#' Management
mod1 <- gamlss(proportion ~ management,
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  )

#' Biology
mod2 <- gamlss(proportion ~ management +
    yelloweye.rockfish + darkblotched.rockfish + Pacific.ocean.perch + bocaccio,
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  )

#' Economics
mod3 <- gamlss(
  proportion ~ management +
    Speed + Crew + Fuel + Variable.costs + Fixed.costs,
    # Fuel + Crew + TotCostNetRev + VarCostNetRev + Speed,
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  )

#' All
mod4 <- gamlss(
  proportion ~ management +
    Crew + Fuel + Variable.costs + Fixed.costs + Speed +
    yelloweye.rockfish + Pacific.ocean.perch + darkblotched.rockfish + bocaccio,
  sigma.fo = ~ random(portgrp), nu.fo = ~ random(portgrp),
  data = nona, family = BEOI,
  control = control, i.control = i.control
  )

# AIC formulas
models <- list(mod0, mod1, mod2, mod3, mod4)
aicc <- mapply(AICc, models)
best <- which.min(aicc)
summary(models[[best]])
plot(models[[best]])

###############################################################################
###############################################################################
#' Look at model structure for each covariate given the catch share has already
#' been implemented. Model results are not reported just looked at in the script
#' for a comparison
###############################################################################
###############################################################################
modstep <- stepGAIC(gamlss(proportion ~ 1,
  sigma.fo = ~ random(portgrp, lambda = 1e-05),
  nu.fo = ~ random(portgrp, lambda = 1e-05),
  data = subset(nona, year > 2010), family = BEOI,
  control = control,
  i.control = glim.control(cc = 0.001, cyc = 50,
  bf.cyc = 800, bf.tol = 0.001)
  ), direction = "forward",
  scope = list(
    lower = ~1,
    upper = ~ yelloweye.rockfish + Pacific.ocean.perch + darkblotched.rockfish +
    bocaccio #+ Variable.costs + Fixed.costs + Crew + Speed + Fuel
    )
  )
exp(coef(modstep))

###############################################################################
allnames <- data.frame(names(coef(models[[length(models)]])))
allcoef <- sapply(models, "[", "mu.coefficients")
for (i in seq(allcoef)) {
  use <- match(names(allcoef[[i]]),
    allnames[, 1])
  allnames[use, i + 1] <- allcoef[[i]]
}
allpars <- allnames
colnames(allpars) <- c("parameter", "intercept", "manage", "biology", "economics", "all")
allpars$parameter <- gsub("\\(|\\)", "", allpars$parameter)
allpars$parameter <- gsub("\\.", " ", allpars$parameter)
sink(file.path(dir.results, "pars.tex"))
print(xtable(allpars, digits = 5), include.rownames = FALSE)
sink()
system(paste("pandoc", file.path(dir.results, "pars.tex"), "-o", file.path(dir.results, "pars.docx")))

###############################################################################
dfpars <- sapply(models, "[", c("mu.df", "sigma.df", "nu.df"))
dfpars <- rbind(dfpars, "AICc" = mapply(AICc, models))
colnames(dfpars) <- colnames(allpars)[-1]
rownames(dfpars) <- c("a", "b", expression(nu), "AICc")
sink(file.path(dir.results, "dof.tex"))
print(xtable(dfpars, digits = 2))
sink()
system(paste("pandoc", file.path(dir.results, "dof.tex"), "-o", file.path(dir.results, "dof.docx")))

###############################################################################
table <- summary(models[[best]])
table <- table[, -which(colnames(table) == "t value")]
sink(file.path(dir.results, "best.tex"))
print(xtable(table, digits = 5), include.rownames = FALSE)
sink()
system(paste("pandoc", file.path(dir.results, "best.tex"), "-o", file.path(dir.results, "best.docx")))
