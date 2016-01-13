#' ----
#' Title: Partitioning analysis of catches by gear type
#' Author: Kelli Faye Johnson
#' ----

#' Read in the catch data as provided for the stock assessment
#' Catches are grouped according to gear that was used while fishing
#' not permit type held while fishing.
catch <- read.csv(file.path(dir.data, "catchbyyearfleet.csv"))
catch <- aggregate(catch ~ year + fleet, data = catch, sum)
catch <- reshape(catch, direction = "wide", timevar = "fleet", idvar = "year")

#' Change absolute catches into proportion of the total catch for each gear
props <- data.frame("year" = catch[, 1],
  prop.table(as.matrix(catch[, -1]), margin = 1))
props <- subset(props, year >= 1982)

#' Calculate the distance between each observation
res <- dist(props)
mod <- hclust(res)

#' Create the figure used in the publication
png(file.path(dir.results, "dendrogram.png"),
  res = resolution, width = width, height = height / 2)
par(mar = c(2, 2.5, 0.25, 0.25), las = 1)
plot(mod, labels = props$year,
  xlab = "", ylab = "", main = "")
mtext(side = 1, line = 0.5, "year")
rect.hclust(mod, k = 6, border = 1)
dev.off()
