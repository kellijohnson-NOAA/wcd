#' ----
#' Title: Plots for west coast drivers
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----
###############################################################################
#### Figure: sablefishlandings
####
###############################################################################
line <- 0.75
plotdata <- aggregate(catch ~ year + fleet, data = data.landbygear, sum)
colnames(plotdata) <- c("year", "Gear", "landings")
plotdata <- reshape(plotdata, direction = "wide",
  idvar = "Gear", timevar = "year")
colnames(plotdata) <- gsub("landings\\.", "", colnames(plotdata))

png(file.path(dir.results, "sablefishlandings.png"),
  res = resolution, width = width, height = height)
colors <- grey.colors(3, start = 0.1, end = 0.7)
points <- 15:17
layout <- matrix(c(1, 1, 2, 1, 1, 2), ncol = 2)
layout(layout)
limits <- c(0.2, 1.2)
par(mar = c(0, 4.5, 1, 1), las = 1)
mp <- barplot(as.matrix(plotdata[, -1]), xaxt = "n",
  ylab = "landings (mt)", , col = colors)
legend(x = 0, y = max(plotdata[, -1])/2, legend = c("hook and line", "pot", "trawl"),
  bty = "n", pch = points, col = colors, cex = 1.5)
select <- seq(0, (floor(dim(plotdata)/5)*5)[2], 10) + 1
xlim <- par("usr")[1:2]
lines(mp[match(data.acl$Year, colnames(plotdata)[-1])], data.acl$ACL,
  lwd = line)
par(mar = c(4.5, 4.5, 0.5, 1))
mp <- barplot(as.matrix(plotdata[, -1]), xaxt = "n",
  col = "white", ylim = c(0, 1), border = NA,
  xlab = "year", ylab = "proportion")
axis(1, at = mp[select], labels = colnames(plotdata)[select + 1])
lines(mp[match(props$year, colnames(plotdata))], props$HKL,
 type = "o", pch = points[1], lwd = 0.5, col = colors[1])
lines(mp[match(props$year, colnames(plotdata))], props$POT,
 type = "o", pch = points[2], lwd = 0.5, col = colors[2])
lines(mp[match(props$year, colnames(plotdata))], props$TWL,
 type = "o", pch = points[3], lwd = 0.5, col = colors[3])
dev.off()

###############################################################################
#### Figure: distribution
####
###############################################################################
betafit <- MASS::fitdistr(data.match.fixed$proportion, "beta",
  start = list(shape1 = 2, shape2 = 2), lower = 0.000001)
betapars <- c(
  "mean" = sum(betafit$estimate[1]) / sum(betafit$estimate),
  "precision" = prod(betafit$estimate)/
    (sum(betafit$estimate)^2 * (sum(betafit$estimate) + 1))
  )

png(file.path(dir.results, "distribution.png"),
  res = resolution, width = width, height = height)
layout <- matrix(c(1, 2, 1, 2, 3, 3), ncol = 3)
layout(layout)
limits <- c(0.2, 1.2)
par(mar = c(0, 4, 1, 1), las = 1)
qqplot(data.match.fixed$proportion,
  rnorm(1000, mean = mean(data.match.fixed$proportion),
  sd = sd(data.match.fixed$proportion)),
  main = "", xlim = limits, ylim = limits,
  ylab = "Normal quantiles", xlab = "", xaxt = "n")
abline(a = 0, b = 1)
par(mar = c(4, 4, 0, 1), las = 1)
qqplot(data.match.fixed$proportion,
  rbeta(1000, betafit$estimate[1], betafit$estimate[2]),
  xlab = "Sample Quantiles",
  ylab = "Beta quantiles", xlim = limits, ylim = limits)
abline(0, 1)
par(mar = c(4, 0.5, 1, 3), las = 1)
hist(data.match.fixed$proportion, main = "", yaxt = "n",
  xlab = "proportion fixed-gear landings", ylab = "")
axis(side = 4, at = 0:3, labels = 0:3)
mtext(side = 4, line = 1, "Frequency", las = 3, cex = 0.75)
dev.off()

###############################################################################
#### Figure: landings_trawl
####
###############################################################################
plotdata <- data.md
plotdata[plotdata$year < 2011 & plotdata$GEAR == "Fixed gear", "land"] <- NA
text <- data.frame("year" = 2016,
  "landings" = unlist(data.acl[data.acl$Year == 2014, c("ACL", "ACL_N", "letrawl")]),
  "text" = c("(a)", "(b)", "(c)"))
plotdata$portgrp <- factor(plotdata$portgrp,
  levels = levels(plotdata$portgrp),
  labels = levels(plotdata$portgrp)[portgrouporder])
plotdata$land <- plotdata$land / 2204.62

png(file.path(dir.results, "landings_trawl.png"),
  res = resolution, width = width / 2, height = height)
ggplot(plotdata) + theme +
  geom_point(aes(x = year, y = land, group = GEAR, pch = GEAR)) +
  facet_grid(portgrp ~ .) +
  geom_text(aes(x = Inf, y = Inf, label = portgrp),
    hjust = 1, vjust = 1, cex = 2.5) +
  theme(legend.position = c(0.17, 0.95),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.key.height = unit(0.5, "mm"),
    legend.key.width = unit(0.5, "cm")) +
  geom_vline(xintercept = 2011, lty = 2) +
  ylab("US West Coast LE trawl sablefish landings (mt)")
dev.off()

###############################################################################
#### Table: priceperpound
####
###############################################################################
png(file.path(dir.results, "priceperpound.png"),
  res = resolution, width = width, height = height / 2)
priceperpound <- data.price[data.price$PACFIN_SPECIES_COMMON_NAME == "SABLEFISH", ]
priceperpound <- priceperpound[,
  c("LANDING_YEAR", "TRAWL_PPP", "HOOK_LINE_PPP", "POT_TRAP_PPP")]
plot(priceperpound$LANDING_YEAR, priceperpound$HOOK_LINE_PPP, col = colors[1],
  ylim = c(0, max(priceperpound[, -1])), las = 1, yaxt = "n", type = "o",
  xlab = "year", ylab = "ex-vessel price per pound ($)", pch = points[1])
lines(priceperpound$LANDING_YEAR, priceperpound$POT_TRAP_PPP,
  col = colors[2], pch = points[2], type = "o")
lines(priceperpound$LANDING_YEAR, priceperpound$TRAWL_PPP,
  col = colors[3], pch = points[3], type = "o")
axis(side = 2, at = (locs <- seq(0, ceiling(max(priceperpound[, -1])), by = 0.50)),
  labels = format(locs, nsmall = 2L), las = 1)
legend(x = min(priceperpound$LANDING_YEAR), y = max(priceperpound[, -1]) * 0.98,
  legend = c("hook and line", "pot", "trawl"),
  bty = "n", pch = points, col = colors)
dev.off()

#EndOfFile
