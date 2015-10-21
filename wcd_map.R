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
data.plot <- data.srvy
colnames(data.plot)[grep("Pacific\\.oc", colnames(data.plot))] <- "POP"
colnames(data.plot)[grep("LAT", colnames(data.plot))] <- "Y"
colnames(data.plot)[grep("LON", colnames(data.plot))] <- "X"

data.plot <- data.plot[,
  c(which(colnames(data.plot) %in% c("X", "Y", "PROJECT_CYCLE")),
    (max(grep("_", colnames(data.plot))) + 1):NCOL(data.plot))]

temp <- data.plot[, c("X", "Y", "PROJECT_CYCLE")]
temp <- lapply((NCOL(temp) + 1):NCOL(data.plot), function(x) {
  data.frame(temp, "weight" = data.plot[, x],
    "species" = gsub("\\.rockfish", "", colnames(data.plot))[x])
})
data.plot <- do.call("rbind", temp)
rm(temp)

colnames(data.plot)[grep("PROJECT_CYCLE", colnames(data.plot))] <- "year"
data.plot$year <- as.numeric(sapply(strsplit(as.character(data.plot$year),
  " "), "[", 2))
data.plot <- subset(data.plot, weight > 0)

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
