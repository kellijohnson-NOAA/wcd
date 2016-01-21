#' ----
#' Title: Tables for west coast drivers
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----
###############################################################################
#### Table: econ
####
###############################################################################
econstuff <- aggregate(
  cbind(Fixed.costs, Variable.costs, Crew, Fuel, Speed) ~ portgrp,
  data = subset(data.md, portgrp != "Monterey and Morro Bay"), mean)
econstuff <- cbind(aggregate(proportion ~ portgrp, nona, length),
  econstuff[, -1])
econstuff <- econstuff[portgrouporder, ]
colnames(econstuff) <- gsub("proportion", "n", colnames(econstuff))
colnames(econstuff) <- gsub("\\.", " ", colnames(econstuff))
colnames(econstuff) <- gsub("grp", " group", colnames(econstuff))

sink(file.path(dir.results, "econ.tex"))
print(xtable(econstuff, digits = 2), include.rownames = FALSE)
sink()
system(paste("pandoc", file.path(dir.results, "econ.tex"), "-o",
  file.path(dir.results, "econ.docx")))
