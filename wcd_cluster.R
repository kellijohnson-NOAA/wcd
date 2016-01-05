
setwd("c:/wcd/data")
catch <- read.csv("catchbyyearfleet.csv")
catch <- aggregate(catch ~ year + fleet, data = catch, sum)
catch <- (reshape(catch, direction = "wide", timevar = "fleet", idvar = "year"))

test <- data.frame("year" = catch[, 1], 
  prop.table(as.matrix(catch[, -1]), margin = 1))
test <- subset(test, year >= 1982)
res <- dist(test)

mod <- hclust(res)
plot(mod, labels = test$year)
rect.hclust(mod, 6)

