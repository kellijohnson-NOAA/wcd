#' ----
#' Title: beta regression on matched data
#' Author: Kelli Faye Johnson
#' Date: 2016-03-22
#' ----

###############################################################################
###############################################################################
#### gamlss models
###############################################################################
###############################################################################
knots <- 2; df <- 2; stop <- 20;
test <- gamboostLSS(
  formula = list(
    "mu" = proportion ~
      # brandom(fyear, df = df) +
      bols(portgrp, intercept = TRUE) +
      bols(Number.of.vessels, intercept = FALSE) +
      # bols(Days, intercept = FALSE) +
      bols(Crew, intercept = FALSE) +
      bols(Fuel, intercept = FALSE) +
      bols(HorsePower, intercept = FALSE) +
      bols(FuelCapacity, intercept = FALSE) +
      bols(Length, intercept = FALSE) +
      bols(Fixed.costs, intercept = FALSE) +
      bols(Variable.costs, intercept = FALSE) +
      bols(sablefish, intercept = FALSE),
  "phi" = proportion ~ bols(INT, intercept = FALSE)),
  families = BetaLSS(),
  control = boost_control(mstop = c(mu = stop, phi = stop)),
  data = droplevels(data.frame(data.match.fixed, "INT" = 1)))
  par(mfrow = c(3, 3)); plot(test)

###############################################################################
###############################################################################
#### beta regression models
###############################################################################
###############################################################################
mod <- betareg(proportion ~ portgrp,
  data = data.match.fixed)
mod1 <- betareg(proportion ~ Number.of.vessels + portgrp,
  data = data.match.fixed)
mod2 <- betareg(proportion ~ sablefish + portgrp,
  data = data.match.fixed)
mod3 <- betareg(proportion ~ portgrp + sablefish + Number.of.vessels,
  data = data.match.fixed)
linear <- lm(proportion ~ portgrp + sablefish + Number.of.vessels,
  data = data.match.fixed)

###############################################################################
###############################################################################
#### coefficients
###############################################################################
###############################################################################
mod3res <- do.call("rbind", summary(mod3)$coefficients)
rownames(mod3res)[1] <- levels(data.match.fixed$portgrp)[1]
rownames(mod3res) <- gsub("portgrp|\\(|\\)", "", rownames(mod3res))
rownames(mod3res) <- gsub("\\.", " ", rownames(mod3res))
mod3res <- data.frame(mod3res[, 1:2], round(mod3res[, NCOL(mod3res)], 2))
colnames(mod3res) <- c("estimate", "std error", "p value")
sink(file.path(dir.results, "glm_coeff.tex"))
print(xtable(mod3res, digits = 2), include.rownames = TRUE,
  sanitize.text.function = function(x){x})
sink()
system(paste("pandoc", file.path(dir.results, "glm_coeff.tex"), "-o",
  file.path(dir.results, "glm_coeff.docx")))

###############################################################################
###############################################################################
#### print results to the screen if verbose == TRUE
###############################################################################
###############################################################################
if (verbose) {
  AIC(mod, mod1, mod2, mod3, mod4, mod5)
  summary(mod3)
  coef(mod3)
  plot(mod3)
  lmtest::lrtest(mod3, mod)
  lmtest::lrtest(mod3, mod1)
  lmtest::lrtest(mod3, mod2)

  par(mfrow = c(3, 3)); plot(test)
  coef(test)
}
