#' ----
#' Title: Tables for west coast drivers
#' Author: Kelli Faye Johnson
#' Date: 2016-01-14
#' ----
###############################################################################
#### Table: econ
####
###############################################################################
econstuff <- stats::aggregate(
  letrawl ~ portgrp + GEAR,
  data = rbind(trawl, fixed), length)
econstuff <- reshape(econstuff, direction = "wide",
  timevar = "GEAR", idvar = "portgrp")
colnames(econstuff) <- c("port group", "trawl", "fixed gear")
levels(econstuff$"port group") <- levels(data.md$portgrp)[portgrouporder]
econstuff <- econstuff[order(econstuff$"port group"), ]

sink(file.path(dir.results, "econ.tex"))
print(xtable(econstuff, digits = 2), include.rownames = FALSE)
sink()
system(paste("pandoc", file.path(dir.results, "econ.tex"), "-o",
  file.path(dir.results, "econ.docx")))

# EndOfFile
