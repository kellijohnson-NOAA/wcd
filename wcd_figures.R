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
plotdata$gear <- factor(plotdata$Gear, levels = unique(plotdata$Gear),
  labels = c("hook and line", "pot", "trawl"))
text <- data.frame("year" = 2016,
  "landings" = unlist(data.acl[data.acl$Year == 2014, c("ACL", "ACL_N", "letrawl")]),
  "text" = c("(a)", "(b)", "(c)"))

g <- ggplot() + theme +
  geom_bar(data = plotdata,
  aes(x = year, y = landings, fill = Gear), stat = "identity") +
  geom_line(data = data.acl, aes(x = Year, y = ACL),     lty = 1, lwd = line) +
  # geom_line(data = data.acl, aes(x = Year, y = ACL_N),   lty = 1, lwd = line) +
  # geom_line(data = data.acl, aes(x = Year, y = letrawl), lty = 1, lwd = line) +
  # geom_text(data = text, aes(x = year, y = landings, label = text)) +
  scale_fill_grey(start = 0.5, end = 0.9) +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
ggsave(filename = "sablefishlandings.png", g, path = dir.results,
  dpi = 300, limitsize = TRUE)

###############################################################################
#### Figure: distribution
####
###############################################################################
temp <- data.md[data.md$portgrp != "Monterey and Morro Bay", ]
betafit <- MASS::fitdistr(temp$letprop[!is.na(temp$letprop)], "beta",
  start = list(shape1 = 0.5, shape2 = 0.5))
betapars <- c(
  "mean" = sum(betafit$estimate[1]) / sum(betafit$estimate),
  "precision" = prod(betafit$estimate)/
    (sum(betafit$estimate)^2 * (sum(betafit$estimate) + 1))
  )

png(file.path(dir.results, "distribution.png"),
  res = resolution, width = width, height = height)
layout <- matrix(c(1, 2, 1, 2, 3, 3), ncol = 3)
layout(layout)
par(mar = c(4, 4, 1, 1), las = 1)
qqplot(rnorm(NROW(temp)), temp$letprop, main = "",
   ylab = "Sample Quantiles", xlab = "Normal quantiles")
qqline(temp$letprop)
qqplot(rbeta(NROW(temp), betafit$estimate[1], betafit$estimate[2]),
  temp$letprop, xlab = "Beta quantiles", ylab = "Sample Quantiles")
abline(0, 1)
hist(temp$letprop, main = "", xlab = "prop landings from LE trawl gear")
dev.off()
rm(temp)
