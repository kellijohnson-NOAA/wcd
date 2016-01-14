#' ----
#' Title: West coast drivers mainipulation of data before running any model
#' Author: Kelli Faye Johnson
#' Date: 2016-01-12
#' ----

#' order of port groups
portgrouporder <- c(9, 1, 7, 3, 2, 4, 5, 8, 6)
data.index <- read.csv(file.path(dir.results, file.index))
data.index$strat <- factor(data.index$strat, levels(data.index$strat),
  levels(data.cost$portgrp)[portgrouporder])
colnames(data.index)[which(colnames(data.index) == "strat")] <- "portgrp"
good <- c("portgrp", "year", "GEAR")
#' Scale down the index data by 1000
docolumns <- !colnames(data.index) %in% c("year", "portgrp")
data.index[, docolumns] <- data.index[, docolumns] / 1000000

colnames(data.cost)[which(colnames(data.cost) == "YEAR")] <- "year"
data.cost <- split(data.cost, data.cost$GEAR)$Trawl
days.days <- split(data.days, data.days$GEAR)$Trawl
data.cost[data.cost == -999] <- NA
data.days[data.days == -999] <- NA
colnames(data.days)[1] <- tolower(colnames(data.days))[1]
colnames(data.netrev)[1] <- tolower(colnames(data.netrev))[1]

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

#### Add in acl data
head(data.md)
head(data.acl)
test <- merge(data.md, data.acl, by.x = "year", by.y = "Year")
test$y <- test$land / (test$letrawl * 2204.62)
hist(test$y)
qqnorm(test$y)
qqplot(rnorm(1000), test$y, main = "standard normal Q-Q Plot",
   ylab = "Sample Quantiles")
qqline(test$y)

betafit <- MASS::fitdistr(test$y[!is.na(test$y)], "beta", start = list(shape1 = 0.5, shape2 = 0.5))
qqplot(rbeta(1000, betafit$estimate[1], betafit$estimate[2]),
  test$y, xlab = "Beta quantiles", ylab = "Sample Quantiles")
abline(0, 1)


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
