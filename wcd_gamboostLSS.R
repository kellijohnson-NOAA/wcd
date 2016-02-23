#' ----
#' Title: gamboostLSS
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----

test <- nona
test$INT <- 1
test$year <- factor(test$year)

#' gamboostLSS
#' mstop == selection of values for mstop are normally based on
#' predictive risk, where large mstop values converge to the same
#' solution as the original algorithm, and small values will
#' only include the most important variables.
#' bols == linear
#' bbs == non-linear using P-splines with a B-spline basis
#'   (Schmid and Hothorn, 2008)
#' brandom == random effects
#' bspatial == spatial or Markov random fields
#' df correspond to the amount of smoothing with smaller values
#'   corresponding to more smoothing and weaker base-learners
df <- 3
knots <- 20
stop <- 7
model1 <- gamboostLSS(
  formula = letprop ~
      # bols(INT, intercept = FALSE) +
      # bols(management, portgrp, intercept = FALSE) +
      bols(management, intercept = FALSE) +
      bols(portgrp, intercept = TRUE) +
      brandom(year, df = df) +
      bbs(buyercount, df = df, knots = knots) +
      bbs(Fixed.costs, df = df, knots = knots) +
      bbs(Variable.costs, df = df, knots = knots) +
      bols(Number.of.vessels.x, intercept = FALSE) +
      # bbs(Number.of.vessels.x, df = df, knots = knots) +
      # bols(Crew, intercept = FALSE) +
      bbs(Crew, df = df, knots = knots) +
      bbs(Fuel, df = df, knots = knots) +
      bbs(Speed, df = df, knots = knots) +
      # bols(Days, intercept = FALSE) +
      bbs(Days, df = df, knots = knots) +
      bbs(FuelCapacity, df = df, knots = knots) +
      bbs(HorsePower, df = df, knots = knots) +
      bbs(Length, df = df, knots = knots) +
      bbs(bocaccio, df = df, knots = knots) +
      bbs(darkblotched.rockfish, df = df, knots = knots) +
      bbs(Pacific.ocean.perch, df = df, knots = knots) +
      bbs(sablefish, df = df, knots = knots),
  families = BetaLSS(),
  data = test,
  control = boost_control(mstop = c(mu = stop, phi = stop)))

model1$mu$offset
model1$phi$offset

ylim <- c(0, 0.3)
pdf(file.path(dir.results, "model1.pdf"))
plot(model1)
plot(predint(model1, pi = c(0.90), which = "Number.of.vessels.x"), ylim = ylim)
plot(predint(model1, pi = c(0.90), which = "Crew"), ylim = ylim)
dev.off()

###############################################################################
###############################################################################
#### port group
###############################################################################
###############################################################################
temp <- data.frame(
  "port group" = gsub("\\(Intercept\\)",
    my.portgroups[order(my.portgroups)][1],
    gsub("portgrp", "",
    names(coef(model1, which = "portgrp", parameter = "mu")[[1]]))),
  "mu" = coef(model1, which = "portgrp", parameter = "mu")[[1]],
  "phi" = coef(model1, which = "portgrp", parameter = "phi")[[1]])
colnames(temp) <- c("port group", "$\\mu$", "$\\phi$")
sink(file.path(dir.results, "gam_portgroup.tex"))
print(xtable(temp, digits = 2), include.rownames = FALSE,
  sanitize.text.function=function(x){x})
sink()
system(paste("pandoc", file.path(dir.results, "gam_portgroup.tex"), "-o",
  file.path(dir.results, "gam_portgroup.docx")))

###############################################################################
###############################################################################
#### mu
###############################################################################
###############################################################################
png(file.path(dir.results, "gam_marginalprediction.png"), width = width,
  height = height * 1.15, res = resolution)
par(mfrow = c(2, 1), mar = c(1, 4, 0.2, 0.1), oma = c(1, 1, 0.1, 0.1),
  xpd = TRUE)
plot(predint(model1, pi = c(0.90), which = "Number.of.vessels.x"),
  xlim = c(-2, 3), ylim = c(0, 0.35), yaxs = "i", las = 1,
  ylab = "attainment", xlab = "", main = "", xaxt = "n")
legend("topright", "number of vessels", bty = "n")
plot(predint(model1, pi = c(0.90), which = "Crew"),
  xlim = c(-2.5, 3), ylim = c(0, 0.35), yaxs = "i",
  ylab = "attainment", xlab = "", main = "", las = 1)
legend("topright", "crew size excluding captain", bty = "n")
dev.off()
