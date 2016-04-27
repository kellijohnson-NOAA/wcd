#' ----
#' Title: gamboostLSS
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----

trawl$INT <- 1
fixed$INT <- 1
trawl$Year <- trawl$year
fixed$Year <- fixed$year
trawl$year <- factor(trawl$year)
fixed$year <- factor(fixed$year)

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
stop <- 5
model1 <- gamboostLSS(
  formula = list(
    "mu" = letprop ~
      # bols(INT, intercept = FALSE) +
      # bols(management, portgrp, intercept = FALSE) +
      bols(management, intercept = FALSE) +
      bols(portgrp, intercept = TRUE) +
      brandom(year, df = df) +
      bbs(buyercount, df = df, knots = knots) +
      bbs(Fixed.costs, df = df, knots = knots) +
      bbs(Variable.costs, df = df, knots = knots) +
      # bols(Number.of.vessels, intercept = FALSE) +
      bbs(Number.of.vessels, df = df, knots = knots) +
      # bols(Crew, intercept = FALSE) +
      bbs(Crew, df = df, knots = knots) +
      bbs(Fuel, df = df, knots = knots) +
      bbs(Speed, df = df, knots = knots) +
      # bols(Days, intercept = FALSE) +
      bbs(Days, df = df, knots = knots) +
      bbs(FuelCapacity, df = df, knots = knots) +
      bbs(HorsePower, df = df, knots = knots) +
      bbs(Length, df = df, knots = knots) +
      bbs(darkblotched.rockfish, df = df, knots = knots) +
      bbs(Pacific.ocean.perch, df = df, knots = knots) +
      bbs(sablefish, df = df, knots = knots),
  "phi" = letprop ~ bols(INT, intercept = FALSE)),
  families = BetaLSS(),
  data = trawl,
  control = boost_control(mstop = c(mu = stop, phi = stop)))

par(mfrow = c(3, 3)); plot(model1)
model1$mu$offset
model1$phi$offset


###############################################################################
###############################################################################
#### fixed gear: model2
###############################################################################
###############################################################################
model2 <- gamboostLSS(
  formula = list(
    "mu" = letprop ~
      # bols(INT, intercept = FALSE) +
      bols(portgrp, intercept = TRUE) +
      brandom(year, df = df) +
      bbs(buyercount, df = df, knots = knots) +
      bbs(Fixed.costs, df = df, knots = knots) +
      bbs(Variable.costs, df = df, knots = knots) +
      # bols(Number.of.vessels, intercept = FALSE) +
      bbs(Number.of.vessels, df = df, knots = knots) +
      # bols(Crew, intercept = FALSE) +
      # bbs(Crew, df = df, knots = knots) +
      # bbs(Fuel, df = df, knots = knots) +
      # bbs(Days, df = df, knots = knots) +
      bbs(FuelCapacity, df = df, knots = knots) +
      bbs(HorsePower, df = df, knots = knots) +
      bbs(Length, df = df, knots = knots) +
      bbs(darkblotched.rockfish, df = df, knots = knots) +
      bbs(Pacific.ocean.perch, df = df, knots = knots) +
      bbs(sablefish, df = df, knots = knots),
  "phi" = letprop ~ bols(INT, intercept = FALSE)),
  families = BetaLSS(),
  data = fixed,
  control = boost_control(mstop = c(mu = stop, phi = stop)))

par(mfrow = c(3, 3)); plot(model2)
dev.off()
model1$mu$offset
model1$phi$offset

###############################################################################
###############################################################################
#### mu
###############################################################################
###############################################################################
xlim <- c(-3, 3)
png(file.path(dir.results, "gam_marginalprediction.png"), width = width,
  height = width * 1.15, res = resolution)
par(mfrow = c(2, 2), mar = c(0.4, 0.2, 0.2, 0.1), oma = c(2, 4, 0.2, 0.1),
  xpd = TRUE)
plot(predint(model1, pi = c(0.90), which = "Number.of.vessels"),
  xlim = xlim, ylim = c(0, 0.35), yaxs = "i", las = 1,
  ylab = "", xlab = "", main = "", xaxt = "n")
legend("topright", "number of vessels", bty = "n")
plot(predint(model1, pi = c(0.90), which = "Variable.costs"),
  xlim = xlim, ylim = c(0, 0.35), yaxs = "i", xaxt = "n",
  yaxt = "n", xlab = "", main = "", las = 1, ylab = "")
legend("topright", "variable costs", bty = "n")
# plot(predint(model2, pi = c(0.90), which = "darkblotched.rockfish"),
#   xlim = xlim, ylim = c(0, 0.35), yaxs = "i",
#   ylab = "", xlab = "", main = "", las = 1)
# legend("topright", "darkblotched rockfish", bty = "n")
plot(predint(model2, pi = c(0.90), which = "Variable.costs"),
  xlim = xlim, ylim = c(0, 0.35), yaxs = "i",
  yaxt = "n", xlab = "", main = "", las = 1, ylab = "")
legend("topright", "variable costs", bty = "n")
mtext(side = 2, outer = TRUE, "attainment", line = 3)
dev.off()

###############################################################################
###############################################################################
#### mgcv gam
###############################################################################
###############################################################################
goodgam <- mgcv::gam(letprop ~
  # s(year, bs="re")  +
  # s(darkblotched.rockfish) +
  s(Number.of.vessels) +
  # s(FuelCapacity) +
  s(Variable.costs) +
  portgrp,
  family = mgcv::betar(link="logit"), data = trawl)
plot(goodgam, pages =  1)
summary(goodgam)

fixgam <- mgcv::gam(letprop ~
  s(Variable.costs, k = 3) +
  portgrp,
  family = mgcv::betar(link = "logit"), data = fixed)
plot(fixgam, pages =  1)
summary(fixgam)
dev.off()

###############################################################################
###############################################################################
#### plot gam
###############################################################################
###############################################################################
png(file.path(dir.results, "gam_marginalprediction.png"),
  res = resolution, width = width, height = width)
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(3, 3, 1, 3))
for (ii in 1:3) {
preds <- predict(goodgam, type = "terms", se.fit = TRUE)
fit.up95 <- preds$fit - 1.96 * preds$se.fit
fit.low95 <- preds$fit + 1.96 * preds$se.fit

  if (ii == 1) {
    get <- "Variable\\.costs"
    x <- trawl$Variable.costs
  }
  if (ii == 2) {
    get <- "Number\\.of\\.vessels"
    x <- trawl$Number.of.vessels
  }
label <- goodgam$edf[grepl(get, names(goodgam$edf))]
  if (ii == 3) {
    get <- "Variable\\.costs"
    x <- fixed$Variable.costs
    preds <- predict(fixgam, type = "terms", se.fit = TRUE)
    fit.up95 <- preds$fit - 1.96 * preds$se.fit
    fit.low95 <- preds$fit + 1.96 * preds$se.fit
    label <- fixgam$edf[grepl(get, names(fixgam$edf))]
  }
y <- grep(get, colnames(preds$fit))
plot(x, preds$fit[, y],
  type = "n", lwd = y, las = 1,
  xlim = c(-3, 3), ylim = c(-1, 1.5),
  xaxt = ifelse(ii == 1, "n", "s"),
  yaxt = ifelse(ii == 2, "n", "s"), ylab = "")
mtext(side = 3, line = -1.5,
  paste0("s(", gsub("\\\\|\\.", " ", get), ", ",
  round(sum(label), 2), ")"))
polygon(c(x[order(x)],
        rev(x[order(x)])),
        c(fit.low95[order(x), y],
          rev(fit.up95[order(x), y])), col="grey",
        border=NA)
lines(x, preds$fit[, y],  lwd = 2)
if (ii == 3) {
  plot(1, 1, type = "n", ann = FALSE, xaxt = "n", yaxt = "n",
    bty = "n")
}
if (ii %in% 2:3) mtext(side = 4, line = 0.5,
  ifelse(ii == 2, "trawl gear", "fixed gears"))
}
dev.off()

###############################################################################
###############################################################################
#### port group
###############################################################################
###############################################################################
temp <- data.frame("portgroup" = names(coef(goodgam)),
  "trawl" = coef(goodgam))
rownames(temp) <- NULL
temp <- merge(temp,
  data.frame("portgroup" = names(coef(fixgam)),
  "fixedgear" = coef(fixgam)), all.x = TRUE)
temp <- temp[-grep("s\\(", as.character(temp$portgroup)), ]
temp$portgroup <- gsub("\\(Intercept\\)",
    my.portgroups[order(my.portgroups)][1], gsub("portgrp", "", temp$portgroup))
temp[-1, 2:3] <- do.call("rbind",
  apply(temp[-1, 2:3], 1, function(x) x + temp[1, 2:3]))
temp$portgroup <- factor(temp$portgroup,
  levels = unique(data.md$portgrp)[order(unique(data.md$portgrp))][portgrouporder])
temp <- temp[order(temp$portgroup), ]
colnames(temp) <- c("port group", "trawl", "fixed gear")
sink(file.path(dir.results, "gam_portgroup.tex"))
print(xtable(temp, digits = 2), include.rownames = FALSE,
  sanitize.text.function = function(x){x})
sink()
system(paste("pandoc", file.path(dir.results, "gam_portgroup.tex"), "-o",
  file.path(dir.results, "gam_portgroup.docx")))
rm(temp)

#EndOfFile
