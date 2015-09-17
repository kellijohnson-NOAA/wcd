#' ----
#' Title: Spatial data regarding US West Coast groundfish fisheries
#' Author: Kelli Faye Johnson
#' ----
#+ setup, include = FALSE
# set global chunk options
opts_chunk$set(fig.path = "figure/map-", fig.align = "center",
  fig.show = "hold")
options(formatR.arrow = TRUE, width = 90)

dir.old <- getwd()
load(file.path("data", "wcd_data_raw.Rdata"))
dir.create("maps", showWarnings = FALSE)

#' Goal - How the fishery responds to multiple socio-ecological drivers?
#'
#' Species -
#' * cowcod - not included b/c minimal to zero bycatch
#' * Pacific Ocean Perch (POP) - rebuilt but included b/c high bycatch
#'
#+ raw_survey, echo = FALSE, warning = FALSE, include = FALSE, cache = TRUE, message = FALSE>>=
my.spp <- c("bocaccio", "canary", "darkblotch", "POP",
  "sablefish", "widow", "yelloweye")
my.spp <- c("darkblotch", "POP","sablefish")
colnames(data.bio)[which(colnames(data.bio) == "Pacific.oc")] <- "POP"
sppgood <- sapply(strsplit(colnames(data.bio), "\\."), "[", 1)
data.need <- data.bio[, c(which(colnames(data.bio) %in% c("X", "Y", "Survey_Cyc")),
  which(sppgood %in% my.spp == TRUE))]

data.plot <- reshape(data.need, idvar = c("X", "Y", "Survey_Cyc"),
  varying = list(4:NCOL(data.need)), v.name = "weight", direction = "long")
rownames(data.plot) <- NULL
colnames(data.plot)[3] <- "year"
data.plot$year <- as.numeric(sapply(strsplit(as.character(data.plot$year),
  " "), "[", 2))
data.plot <- subset(data.plot, weight > 0)
data.plot$species <- factor(data.plot$time, levels = 1:length(my.spp),
  labels = colnames(data.need)[-c(1:3)])

#+plot_raw_survey, echo = FALSE, warning = FALSE, message = FALSE
p <- ggmap(get_map("Redding, California", maptype = "terrain", zoom = 5, source = "google"))
p +
  facet_grid(species ~ year) +
  geom_point(data = data.plot, aes(x = X, y = Y, size = weight / 10), alpha = 0.25) +
  theme(text = element_text(size = 12)) +
  scale_alpha(guide = FALSE) + scale_size(guide = FALSE) + xlab("") + ylab("")

p +
  facet_grid(species ~ year) +
  geom_point(data = subset(data.plot, species == "sablefish"), aes(x = X, y = Y, size = weight / 10), alpha = 0.25) +
  theme(text = element_text(size = 12)) +
  scale_alpha(guide = FALSE) + scale_size(guide = FALSE) + xlab("") + ylab("")

#'Raw landed weight of sablefish and petrale from the Northwest Fisheries
#'Science Center Shelf-Slope Survey. The size of the circle is proportional to the
#'landed weight, with circles being semi-transparent to show tows within close
#'proximity of each other.

#+workdir, echo = FALSE
setwd(dir.old)
