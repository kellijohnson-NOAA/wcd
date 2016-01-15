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
stop <- 25
model1 <- gamboostLSS(
  formula = letprop ~
      # bols(INT, intercept = FALSE) +
      # bols(management, portgrp, intercept = FALSE) +
      bols(management, intercept = FALSE) +
      bols(portgrp, intercept = TRUE) +
      brandom(year, df = 4) +
      bbs(Crew, df = df, knots = knots) +
      # bbs(Days, df = df, knots = knots) +
      bols(Days, intercept = FALSE) +
      bbs(Fixed.costs, df = df, knots = knots) +
      bbs(Fuel, df = df, knots = knots) +
      # bbs(Number.of.vessels.x, df = df, knots = knots) +
      bols(Number.of.vessels.x, intercept = FALSE) +
      bbs(Speed, df = df, knots = knots) +
      bbs(Variable.costs, df = df, knots = knots) +
      bbs(bocaccio, df = df, knots = knots) +
      bbs(darkblotched.rockfish, df = df, knots = knots) +
      bbs(Pacific.ocean.perch, df = df, knots = knots) +
      bbs(sablefish, df = df, knots = knots) +
      # bols(sablefish, intercept = FALSE) +
      bbs(yelloweye.rockfish, df = df, knots = knots),
  families = BetaLSS(),
  data = test,
  control = boost_control(mstop = stop))

model1$mu$offset
model1$phi$offset

pdf(file.path(dir.results, "model1.pdf"))
plot(model1)
dev.off()

###############################################################################
###############################################################################
#### port group
###############################################################################
###############################################################################
temp <- data.frame(
  "port group" = unique(test$portgrp),
  "mu" = coef(model1, which = "portgrp", parameter = "mu")[[1]],
  "phi" = coef(model1, which = "portgrp", parameter = "phi")[[1]])
temp <- temp[portgrouporder[-length(portgrouporder)] - 1, ]
colnames(temp) <- c("port group", "$\\mu$", "$\\phi$")
sink(file.path(dir.results, "gam_portgroup.tex"))
print(xtable(temp, digits = 2), include.rownames = FALSE,
  sanitize.text.function=function(x){x})
sink()
system(paste("pandoc", file.path(dir.results, "gam_portgroup.tex"), "-o",
  file.path(dir.results, "gam_portgroup.docx")))

###############################################################################
###############################################################################
#### year
###############################################################################
###############################################################################
temp <- coef(model1, which = "year", parameter = "mu")$brandom
temp <- data.frame("year" = gsub("year", "", names(temp)),
  "random effect" = temp)
sink(file.path(dir.results, "gam_year.tex"))
print(xtable(temp, digits = 7), include.rownames = FALSE,
  sanitize.text.function=function(x){x})
sink()
system(paste("pandoc", file.path(dir.results, "gam_year.tex"), "-o",
  file.path(dir.results, "gam_year.docx")))

#' plot of port group mu and phi
# par(mfrow = c(2, 1), mar = c(0, 4.5, 7, 1), col.axis = "white",
#   tck = 0, las = 1, oma = c(0, 0, 0, 0), xpd = NA)
# plot(model1, which = "portgrp", parameter = "mu",
#   cex.lab = 0.01, xlab = "")
# par(mar = c(7, 4.5, 0, 1), col.axis = "black", tck = -0.02)
# axis(2)
# par(col.axis = "white", tck = 0, las = 1)
# plot(model1, which = "portgrp", parameter = "phi", xlab = "")
# par(col.axis = "black", tck = -0.02, las = 1)
# axis(1, at = 1:length(levels(test$portgrp)), labels = FALSE,
#   outer = TRUE)
# text(x = 1:length(levels(test$portgrp)),
#   y = par()$usr[3] - 0.1 * (par()$usr[4] - par()$usr[3]),
#   labels = levels(test$portgrp), srt = 25, adj = 1, xpd = TRUE,
#   cex = 0.75)
# axis(2)
# mtext(side = 1, line = 5.5, "port group", cex = 0.75)

###############################################################################
###############################################################################
#### mu
###############################################################################
###############################################################################
png(file.path(dir.results, "gam_mu.png"), width = width,
  height = height * 1.15, res = resolution)
par(mfrow = c(3, 1), mar = c(2, 3, 0.5, 1), las = 1, oma = c(1, 0, 0, 0))
plot(model1, which = "Fixed.costs", parameter = "mu", xlab = "",
  main = "", ylab = "")
legend("topright", "fixed costs", bty = "n")
plot(model1, which = "Fuel", parameter = "mu",
  xlab = "", main = "", ylab = "")
legend("topright", "fuel", bty = "n")
plot(model1, which = "darkblotched.rockfish", parameter = "mu",
  xlab = "", main = "", ylab = "")
legend("topright", "darkblotched.rockfish", bty = "n")
dev.off()

