###############################################################################
###############################################################################
## Purpose:    West coast drivers of fishermen choice
##             Read in the raw data
## Author:     Kelli Faye Johnson
## Contact:    kellifayejohnson@gmail.com
## Date:       2014-08-26
## Comments:
###############################################################################
###############################################################################

library(xtable)
dir.curr <- getwd()
setwd(dir.results)

data.risk.print <- data.risk[, 1:2]
colnames(data.risk.print) <- c("port group", "vessels (n)")
sink("risk.tex")
print(xtable(data.risk.print, digits = 0), include.rownames = FALSE,
      floating = FALSE)
sink()
system("pandoc risk.tex -o risk.docx")

data.spp.print <- droplevels(
                  subset(data.econ, SPGRP %in% c("Flatfish", "Dover sole"),
                         select = c("SPGRP", "YEAR", "portgrp", "rev",
                                    "vcount", "land", "risk.v", "Fixed.costs",
                                    "Variable.costs", "TotRev", "HorsePower",
                                    "Length", "bocaccio_mean", "darkblotch_mean",
                                    "Pacific_mean", "Y")))

png("y.png", height = 5, width = 3.5, units = "in", res = 400)
plot.spp <- c("Flatfish", "Dover sole")
par(mfrow = c(length(plot.spp), 1), mar = c(0,0,0,0), oma = c(3,4,0.5,0.5),
    xpd = TRUE)
for(q in seq_along(plot.spp)){
test <- with(subset(data.spp.print, SPGRP == plot.spp[q]),
     tapply(Y, list(YEAR, portgrp), mean, na.rm = TRUE))
matplot(test, type = "l", las = 1, col = 1:9,
        lwd = 2, xaxt = "n", lty = 1,
        ylab = "",
        ylim = c(0, 0.20))
mtext(plot.spp[q], side = 3, line = -2, adj = 0)
abline(v = 2.4, lwd = 3, col = "red")
if(q == 2) {
    axis(1, at = 1:4, unique(data.econ$YEAR))
    mtext("proportion of yearly TAC attained", side = 2, line = 3,
          outer = TRUE)
}
if(q == 1){
    legend("topright", colnames(test), col = 1:9, lty = 1,
           bty = "n", cex = 0.5)
}}
dev.off()




setwd(dir.curr)
