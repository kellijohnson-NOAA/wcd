
long <- function(data, colname, spp = "Sablefish", change = -999) {
  if ("SPGRP" %in% colnames(data)) {
    data <- subset(data, SPGRP == spp)
  }
  vnames <- grep("X", colnames(data), value = TRUE)
  years <- gsub("X", "", vnames)
  temp <- reshape(data, direction = "long", v.names = colname,
    varying = list(vnames))
  temp$year <- factor(temp$time, levels = unique(temp$time),
    labels = years)
  temp <- temp[, -which(colnames(temp) %in% c("time", "id", "SPGRP"))]
  temp[temp[, colname] == -999, colname] <- change
  return(temp)
}
