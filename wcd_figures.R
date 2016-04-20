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
layout <- matrix(c(1, 1, 2, 1, 1, 2), ncol = 2)
layout(layout)
limits <- c(0.2, 1.2)
par(mar = c(0, 4.5, 1, 1), las = 1)
mp <- barplot(as.matrix(plotdata[, -1]), xaxt = "n",
  ylab = "landings (mt)", , col = colors)
legend(x = 0, y = max(plotdata[, -1])/2, legend = c("hook and line", "pot", "trawl"),
  bty = "n", pch = 15:17, col = colors, cex = 1.5)
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
 type = "o", pch = 15, lwd = 0.5, col = colors[1])
lines(mp[match(props$year, colnames(plotdata))], props$POT,
 type = "o", pch = 16, lwd = 0.5, col = colors[2])
lines(mp[match(props$year, colnames(plotdata))], props$TWL,
 type = "o", pch = 17, lwd = 0.5, col = colors[3])
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

g <- ggplot(plotdata) + theme +
  geom_point(aes(x = year, y = land, group = GEAR, pch = GEAR)) +
  facet_grid(portgrp ~ .) +
  geom_vline(xintercept = 2011, lty = 2) +
  geom_text(aes(x = Inf, y = Inf, label = portgrp),
    hjust = 1, vjust = 1, cex = 4) +
  geom_text(data = subset(plotdata, portgrp == "Washington"),
    aes(x = year, y = -Inf, label = letrawl),
    hjust = 0.5, vjust = 0, cex = 4) +
  theme(legend.position = c(0.2, 0.07),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text = element_blank()) +
  ylab("US West Coast LE trawl sablefish landings (mt)")
ggsave(filename = "landings_trawl.png", g, path = dir.results,
  dpi = 300, limitsize = TRUE)
dev.off()

#EndOfFile
