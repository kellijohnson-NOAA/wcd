#' ----
#' Title: West coast drivers model using gamlss with linear fixed effects
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----

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

###############################################################################
###############################################################################
# Model structures
###############################################################################
###############################################################################

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

###############################################################################
###############################################################################
# AIC formulas
###############################################################################
###############################################################################
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
