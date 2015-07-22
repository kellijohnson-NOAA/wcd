
###############################################################################
#### Read data or load data
###############################################################################
source("wcd_readdata.R")

my.spp <- c("Dover.sole", "sablefish", "longspine", "shortspine",
            "bocaccio", "canary.roc", "cowcod", "darkblotch", "lingcod",
            "Pacific.oc", "petrale.so", "widow.rock", "yelloweye")

###############################################################################
#### Manipulate data
###############################################################################
## Combine all of economic data into five main data frames
## 1. data.p.spp: pertains to species data
## 2. data.p.vess: pertains to the vessel data
## 3. data.p.bio: pertains to the survey data
## 4. data.tac.before: TAC measurements before (Peter Kuryiama)
## 5. data.tac.after: TAC measurements after (Watson)
## aggregate these data frames into a single frame data.econ
## data.econ is then altered to make a final data frame
## data.frames with an "l" at the end of the name refer to long format
names.change <- 4:7
names(data.rev)[names.change] <- substring(names(data.rev)[names.change], 2, 5)
names(data.vcount)[names.change] <- substring(names(data.vcount)[names.change], 2, 5)
names(data.land)[names.change] <- substring(names(data.land)[names.change], 2, 5)
names(data.bcount)[names.change - 1] <- substring(names(data.bcount)[names.change -1], 2, 5)
data.rev.l <- reshape2::melt(data.rev, id.vars=c("SPGRP", "portgrp", "GEAR"),
                             value.name = "rev", variable.name = "YEAR")
data.vcount.l <- reshape2::melt(data.vcount, id.vars=c("SPGRP", "portgrp", "GEAR"),
                                value.name = "vcount", variable.name = "YEAR")
data.land.l <- reshape2::melt(data.land, id.vars=c("SPGRP", "portgrp", "GEAR"),
                             value.name = "land", variable.name = "YEAR")
data.bcount.l <- reshape2::melt(data.bcount, id.vars=c("SPGRP", "portgrp"),
                             value.name = "bcount", variable.name = "YEAR")

idvars <- c("SPGRP", "portgrp", "GEAR", "YEAR")
data.p.spp <- merge(data.rev.l, data.vcount.l, by = idvars)
data.p.spp <- merge(data.p.spp, data.land.l, by = idvars)
data.p.spp <- merge(data.p.spp, data.bcount.l, by = idvars[c(1,2,4)])
## Set all negative values to NA
for(q in which(sapply(data.p.spp, class) == "numeric")){
    negvals <- which(data.p.spp[, q] < 0)
    data.p.spp[negvals , q] <- NA
}

idvars <- c("YEAR", "GEAR", "portgrp")
data.p.vess <- merge(data.cost, data.days, by = c(idvars, "Number.of.vessels"))
data.p.vess <- merge(data.cost, data.netrev, by = idvars)
data.p.vess <- merge(data.p.vess, data.vess, by = idvars)
## Set all negative values to NA
for(q in which(sapply(data.p.vess, class) == "numeric")){
    negvals <- which(data.p.vess[, q] < 0)
    data.p.vess[negvals , q] <- NA
}

## Select only the trawl data
data.econ <- subset(merge(data.p.spp, data.p.vess, by = idvars),
                    GEAR == "Trawl")

## Create a table that specifies the risk pool status per port group
data.portgrp <- levels(data.econ$portgrp)
n.years <- length(unique(data.econ$YEAR))
data.risk <- data.frame("portgrp" = rep(data.portgrp, n.years),
                        "YEAR" = rep(unique(data.econ$YEAR), each = length(data.portgrp)))
data.risk$risk.v <- 0
data.risk$risk.v[data.risk$portgrp == "Washington" &
                 data.risk$YEAR %in% 2011:2012] <- 4
data.risk$risk.v[data.risk$portgrp == "San Francisco and Bodega Bay" &
                 data.risk$YEAR %in% 2011:2012] <- 2
data.risk$risk.v[data.risk$portgrp == "Fort Bragg" &
                 data.risk$YEAR %in% 2011:2012] <- 4
data.risk$risk.v[data.risk$portgrp == "Monterey and Morro Bay" &
                 data.risk$YEAR %in% 2011:2012] <- 4
data.risk$risk.f <- ifelse(data.risk$risk.v > 0, "yes", "no")
data.econ <- merge(data.econ, data.risk, by = c("portgrp", "YEAR"))

## Manipulate the survey data
# Remove "Cycle" from the survey year
data.bio$YEAR <- do.call("rbind",
                         strsplit(as.character(data.bio$Survey_Cyc),
                                  " "))[, 2]
names(data.bio)[match("Port_Group", names(data.bio))] <- "portgrp"
data.bio <- subset(data.bio, select = c(my.spp, "portgrp", "YEAR"))
data.numtrawl <- as.data.frame(table(data.bio$YEAR, data.bio$portgrp))
colnames(data.numtrawl) <- c("YEAR", "portgrp", "numtrawl")
data.bio.l <- reshape2::melt(data.bio, id.vars = c("YEAR", "portgrp"),
                             value.name = "CPUE", variable.name = "Species")
data.bio.l$present <- ifelse(data.bio.l$CPUE > 0, 1, 0)
data.bio.l <- merge(data.bio.l, data.numtrawl, idvars = c("YEAR", "portgrp"))

data.bio.s <- sapply(split(data.bio.l$CPUE, data.bio.l[, c("YEAR", "portgrp", "Species")]),
                     function(x) {
                         my.mean <- mean(x, na.omit = TRUE)
                         my.median <- median(x)
                         my.pos <- sum(which(x > 0)) / length(x)
                         my.length <- length(x)
                         return(c(my.mean, my.pos, my.length))
                     })
data.temp <- strsplit(colnames(data.bio.s), ".", fixed = TRUE)
for(q in 1:length(data.temp)){
    if(length(data.temp[[q]]) > 3){
        data.temp[[q]] <- data.temp[[q]][1:3]
    }
}
data.temp <- do.call("rbind", data.temp)
data.bio.s <- as.data.frame(cbind(data.temp, t(data.bio.s)))
colnames(data.bio.s) <- c("YEAR", "portgrp", "spp", "mean", "poc", "numtrawl")

data.bio.w <- reshape2::recast(data.bio.s,
                              YEAR + portgrp ~ spp + variable,
                              measure.var = c("mean", "poc"))
temp.id <- grep("_", names(data.bio.w))
data.bio.w[ , temp.id] <- as.numeric(unlist(data.bio.w[, temp.id]))
data.econ <- merge(data.econ, data.bio.w, idvars = c("YEAR", "portgrp"))

## Manipulate TAC data
names(data.tac.after) <- c("Species", "2011NW", "2011W", "2011", "2011Alloc", "2011Attain",
                                      "2012NW", "2012W", "2012", "2012Alloc", "2012Attain",
                           "annualdiff", "attaindiff")
data.tac.after$Subregion <- NA

#' replace species names in data$Species column
#'
#' The original data set names do not match Peter Kuryiama's data
#' @param data The data
#' @param old.name The original species name
#' @param new.name The name you want to change  it to
#' @return The data with new names and subregion entries if applicable
#' @export
change.spp <- function(data, old.name, new.name){
    levels(data$Species)[levels(data$Species) == old.name] <- new.name
    if(grepl("north", old.name, ignore.case = TRUE)) {
        data[data$Species == new.name, "Subregion"] <- "north"
    }
    if(grepl("south", old.name, ignore.case = TRUE)) {
        data[data$Species == new.name, "Subregion"] <- "south"
    }
    return(data)
}
data.tac.after <- change.spp(data.tac.after, "Bocaccio rockfish South of 40", "Bocaccio rockfish")
data.tac.after <- change.spp(data.tac.after, "Chilipepper rockfish South of 40", "Chilipepper rockfish")
data.tac.after <- change.spp(data.tac.after, "Cowcod South of 40", "Cowcod rockfish")
data.tac.after <- change.spp(data.tac.after, "Longspine thornyheads North of 34", "Longspine thornyhead rockfish")
data.tac.after <- change.spp(data.tac.after, "Yellowtail rockfish North of 40", "Yellowtail rockfish")
data.tac.after <- change.spp(data.tac.after, "Splitnose rockfish South of 40", "Splitnose rockfish")
data.tac.after <- change.spp(data.tac.after, "Pacific ocean perch North of 40", "Pacific ocean perch")
data.tac.after <- change.spp(data.tac.after, "Sablefish North of 36", "Sablefish")
data.tac.after <- change.spp(data.tac.after, "Sablefish South of 36", "Sablefish")
data.tac.after <- change.spp(data.tac.after, "Shortspine thornyheads North of 34", "Shortspine thornyhead rockfish")
data.tac.after <- change.spp(data.tac.after, "Shortspine thornyheads South of 34", "Shortspine thornyhead rockfish")
data.tac.after <- change.spp(data.tac.after, "Yellowtail rockfish ", "Yellowtail rockfish")
data.tac.after <- change.spp(data.tac.after, "Yelloweye", "Yelloweye rockfish")

data.tac.before <- change.spp(data.tac.before, "Chilipepper", "Chilipepper rockfish")
data.tac.before <- change.spp(data.tac.before, "Yellowtail", "Yellowtail rockfish")

data.tac.before$Total.ABC <- as.numeric(as.character(data.tac.before$Total.ABC))
## lbs to metric ton conversion
# 1 lb == 0.00045359237 metric tons
# landings are in lbs where allocation is in metric tons
data.tac.before$Total.ABC <- 2204 * data.tac.before$OY.Total.Catch

data.tac.after$"2011Alloc" <- as.numeric(as.character(data.tac.after$"2011Alloc"))
data.tac.after$"2012Alloc" <- as.numeric(as.character(data.tac.after$"2012Alloc"))

data.temp <- rbind(data.frame(Species = as.character(data.tac.after$Species),
                              Subregion = data.tac.after$Subregion,
                              Total.ABC = data.tac.after$"2011Alloc",
                              Year = rep(2011, dim(data.tac.after)[1])),
                   data.frame(Species = as.character(data.tac.after$Species),
                              Subregion = data.tac.after$Subregion,
                              Total.ABC = data.tac.after$"2012Alloc",
                              Year = rep(2012, dim(data.tac.after)[1])))
colnames(data.temp) <- c("Species", "Subregion", "Total.ABC", "Year")


cols <- intersect(colnames(data.tac.before), colnames(data.temp))
data.tac <- rbind(data.tac.before[, cols], data.temp[,cols])
data.tac <- data.tac[with(data.tac, order(Species, Subregion, Year)), ]
data.tac <- droplevels(subset(data.tac, Species != "Total" & Year %in% 2009:2012))

# Create species groups with TAC
# Todo: make a better listing of groups
spp <- list("Dover sole" = "Dover sole",
            "Flatfish" = c("Arrowtooth flounder", "English sole",
                           "Starry flounder", "Other flatfish"),
            "Other groundfish" = c("Lingcod", "Blue"),
            "Pacific whiting" = "Pacific whiting",
            "Petrale sole" = "Petrale sole",
            "Rockfish" = grep("rockfish", levels(data.tac$Species), ignore.case = TRUE, value = TRUE),
            "Sablefish" = "Sablefish",
            "Sharks, skates and rays" = c("Longnose skate"),
            "Thornyheads" = grep("thornyhead", levels(data.tac$Species), ignore.case = TRUE, value = TRUE))

my.list <- list()
for(q in 1:length(spp)){
    data.temp <- subset(data.tac, Species %in% c(spp[[q]]))
    data.temp <- tapply(data.temp$Total.ABC, data.temp$Year, sum)
    data.temp <- data.frame(YEAR = names(data.temp),
                            Subregion = rep(NA, length(data.temp)),
                            SPGRP = rep(names(spp)[q], length(data.temp)),
                            Total.ABC = data.temp)
    my.list[[q]] <- data.temp
}

idvars <- c("SPGRP", "YEAR")
data.econ <- merge(data.econ, do.call("rbind", my.list), by = idvars)
data.econ$man <- data.econ$YEAR
levels(data.econ$man) <- c("before", "before", "after", "after")
data.econ$YEAR <- as.numeric(as.character(data.econ$YEAR))
data.econ$Y <- with(data.econ, land / Total.ABC)

# linear model
data.md <- subset(data.econ, SPGRP == "Flatfish")
ff.1 <- gamm(Y ~ s(vcount) + man + risk.v + bocaccio_poc,
             correlation = corAR1(form = ~ YEAR | portgrp),
             data = data.md, na.action = na.omit, family = "binomial")

ff.2 <- gamm(Y ~ s(vcount) + man + risk.v + I(rev/land),
             correlation = corAR1(form = ~ YEAR | portgrp),
             data = data.md, na.action = na.omit, family = "binomial")
summary(ff.1$gam)
summary(ff.2$gam)
plot(ff.2$gam)

data.md <- subset(data.econ, SPGRP == "Dover sole")
ds.1 <- gamm(Y ~ s(vcount) + man + risk.v + darkblotch_poc,
             correlation = corAR1(form = ~ YEAR | portgrp),
             data = data.md, na.action = na.omit, family = "binomial")

ds.2 <- gamm(Y ~ vcount + man + risk.v + s(darkblotch_poc),
             correlation = corAR1(form = ~ YEAR | portgrp),
             data = data.md, na.action = na.omit, family = "binomial")
summary(ds.1$gam)
sink("c:\\wcd\\results\\results.tex")
print(xtable::xtable(summary(ds.2$gam)$p.table))
sink()
system('pandoc "c:\\wcd\\results\\results.tex" -o "c:\\wcd\\results\\results.docx"')
plot(ds.2$gam)
library(xtable)
names(data.econ)

###############################################################################
#### Plots
###############################################################################

col.names <- c("rev", "land", "vcount", "bcount")
true.names <- c("revenue", "landings", "vessel count", "buyer count")
port.unique <- levels(data.p.spp$portgrp)
spp.unique <- levels(data.p.spp$SPGRP)

pdf(file.path(dir.results, "spp.pdf"))
for(q in seq_along(spp.unique)){
spp <- unique(data.p.spp$SPGRP)[q]
data.plot.1 <- subset(data.p.spp, SPGRP == spp & GEAR == "Fixed gear")
data.plot.2 <- subset(data.p.spp, SPGRP == spp & GEAR == "Trawl")

par(mfrow = c(4, 2), mar = c(0, 5, 0, 0), oma = c(4, 2, 5, 1), xpd = TRUE)
for(w in seq_along(col.names)){
  col <- match(col.names[w], colnames(data.plot.1))
  port <- unique(data.plot.1$portgrp)[1]
    if(dim(data.plot.1)[1] > 0){
        ylim <- c(0, max(data.plot.1[, col], na.rm = TRUE))
        with(subset(data.plot.1, rev > 0 & portgrp == port),
             plot(YEAR, rev, las = 1, xaxt = "n", ylim = ylim, border = "white"))
               mtext(true.names[w], side = 2, line = 3.5)
            if(w == 1) {
                mtext("Fixed gear", side = 3, line = 1)
                legend("topright", legend = letters[1:length(port.unique)], pch = 1:length(port.unique),
                       bty = "n", horiz = FALSE, xpd = TRUE)
            }
            if(w == length(col.names)){
                axis(1, at = c(1:4), labels = 2009:2012)
                mtext(paste(paste0("(", letters[1:length(port.unique)], ") ",
                            port.unique), collapse = " "),
                      side = 1, line = 3, outer = TRUE, cex = .45)
            }
            for(l in seq_along(port.unique)){
                plot.points <- data.plot.1[data.plot.1$portgrp == port.unique[l], ]
                if(dim(plot.points)[1] == 0) {next}
                points(plot.points[which(plot.points[, col] > 0), col],
                       pch = l, type = "p")
            }} else {
                with(data.plot.2,
                     plot(YEAR, rev, xaxt = "n", yaxt = "n", border = "white"))
                mtext(true.names[w], side = 2, line = 3.5)
                if(w == 1) {
                  mtext("Fixed gear", side = 3, line = 1)
                  legend("topright", legend = letters[1:length(port.unique)], pch = 1:length(port.unique),
                         bty = "n", horiz = FALSE, xpd = TRUE)
            }
            }
    if(dim(data.plot.2)[1] > 0){
        ylim <- c(0, max(data.plot.2[, col], na.rm = TRUE))
        with(subset(data.plot.2, rev > 0 & portgrp == port),
             plot(YEAR, rev, las = 1, xaxt = "n", ylim = ylim, border = "white"))
            if(w == 1) {
                mtext("Trawl", side = 3, line = 1)
                mtext(spp, side = 3, line = 3, outer = TRUE)
            }
            if(w == length(col.names)){
                axis(1, at = c(1:4), labels = 2009:2012)
            }
            for(l in seq_along(port.unique)){
                plot.points <- data.plot.2[data.plot.2$portgrp == port.unique[l], ]
                if(dim(plot.points)[1] == 0) {next}
                points(plot.points[which(plot.points[, col] > 0), col],
                       pch = l, type = "p")
            }    }
}
}
dev.off()
## TODO:
## 2. Deal with overlapping points
