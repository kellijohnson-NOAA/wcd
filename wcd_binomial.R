#' ----
#' Title: gamboostLSS
#' Author: Kelli Faye Johnson
#' Date: 2016-04-20
#' ----

###############################################################################
###############################################################################
#' To create binomial glm
###############################################################################
###############################################################################
data.binom <- data.md
data.binom$fullday <- with(data.binom, Days * Number.of.vessels)
data.binom$fullday <- ceiling(data.binom$fullday)
data.binom <- merge(data.binom,
  aggregate(fullday ~ year + portgrp, data = data.binom, sum),
  by = c("year", "portgrp"))
data.binom <- data.binom[!is.na(data.binom$fullday.x), ]
test <- glm(cbind(fullday.x, fullday.y) ~ portgrp + Variable.costs + Crew,
  family = binomial, data = data.binom[data.binom$GEAR == "Fixed gear", ])
summary(test)

#EndOfFile
