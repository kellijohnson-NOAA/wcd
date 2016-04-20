#' ----
#' Title: Spatial data regarding US West Coast groundfish fisheries
#' Author: Kelli Faye Johnson
#' ----

# Variable inputs
small <- 0.10

#### Data
getstrata <- eval(parse(text =
  grep("tops <-", readLines("wcd_survey.R"), value = TRUE)))
getstrata <- unique(c(getstrata, gsub(")", "",
  strsplit(grep("bottoms <-", readLines("wcd_survey.R"),
  value = TRUE), ", ")[[1]][2])))
mode(getstrata) <- "numeric"
data.yrin <- read.csv(file.path(dir.results, file.index))

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

#### Maps
us <- getData("GADM", country = "USA", level = 1)
canada <- getData("GADM", country = "CAN",level = 1)

us.states <- us[us$NAME_1 %in% c("California", "Oregon", "Washington"), ]
ca.provinces <- canada[canada$NAME_1 %in% c("British Columbia"), ]

us.bbox <- bbox(us.states)
xlim <- c(min(us.bbox[1, 1]) * 1.02, max(us.bbox[1, 2]))
ylim <- c(min(us.bbox[2, 1]) * 0.98, max(us.bbox[2, 2]))


p <- ggplot() +
  theme +
  xlim(xlim) + ylim(ylim) + xlab("") + ylab("")

#### Start individual maps

###############################################################################
###############################################################################
#### Plot of raw survey data for each species, including those not included
#### in the model for relative indexes of abundance due to lack of data.
####
####
###############################################################################
###############################################################################
pp <- p +
  geom_point(data = data.plot[data.plot$species == "sablefish", ],
    aes(x = X, y = Y, size = weight / 8), pch = 1, alpha = 0.2) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6)) +
  geom_path(data = us.states, aes(x = long, y = lat, group = group), size = small) +
  geom_path(data = ca.provinces, aes(x = long, y = lat, group = group), size = small) +
  coord_map() +
  facet_wrap( ~ species) +
  theme(strip.text = element_text(vjust = 5.25),
        panel.margin = unit(c(-0.5, 0, -0.5, 0), "lines"))
ggsave(filename = "abundance.png", pp, path = dir.results,
  dpi = 300, limitsize = TRUE)
###############################################################################

###############################################################################
###############################################################################
#### Plot of study area with the strata for relative indexes of abundance
#### marked with dashed lines and the strata for sablefish management
#### marked with a solid line. Each included city in the port groups are
#### also listed.
###############################################################################
###############################################################################
pp <- p +
  theme(legend.position = "none",
        axis.text = element_text(size = 6)) +
  geom_path(data = us.states, aes(x = long, y = lat, group = group), size = small) +
  geom_path(data = ca.provinces, aes(x = long, y = lat, group = group), size = small) +
  coord_map() +
  geom_hline(yintercept = getstrata, lty = 2) +
  geom_hline(yintercept = 36.0, lty = 1) +
  annotate("text", x = -126.2, y = getstrata[-length(getstrata)] - 1.0,
    label = LETTERS[1:(length(getstrata) - 1)], size = 3) +
  annotate("text", x = -126.2, y = 36.000, label = "sablefish N\nsablefish S", size = 3) +
  annotate("text", x = -126.2, y = 43.090, label = "Cape\nBlanco", size = 3) +
  annotate("text", x = -126.2, y = 40.470, label = "Cape\nMendocino", size = 3) +
  annotate("text", x = -126.2, y = 34.450, label = "Point\nConception", size = 3) +
  annotate("text", x = -122.7, y = 46.189, label = "Astoria", size = 3) +
  annotate("text", x = -122.7, y = 45.455, label = "Tillamook", size = 3) +
  annotate("text", x = -123.0, y = 44.600, label = "Newport", size = 3) +
  annotate("text", x = -123.0, y = 43.376, label = "Coos Bay", size = 3) +
  annotate("text", x = -123.0, y = 42.058, label = "Brookings", size = 3) +
  annotate("text", x = -122.7, y = 41.756, label = "Crescent City", size = 3) +
  annotate("text", x = -123.0, y = 40.802, label = "Eureka", size = 3) +
  annotate("text", x = -122.7, y = 39.446, label = "Fort Bragg", size = 3) +
  annotate("text", x = -121.8, y = 38.324, label = "Bodega Bay", size = 3) +
  annotate("text", x = -120.8, y = 37.783, label = "San Francisco", size = 3) +
  annotate("text", x = -120.8, y = 36.600, label = "Monterey", size = 3) +
  annotate("text", x = -119.8, y = 35.379, label = "Morro Bay", size = 3)
ggsave(filename = "strata.png", pp, path = dir.results, scale = 1,
  width = par("din")[1], height = par("din")[2], units = "in",
  dpi = 300, limitsize = TRUE)
dev.off()
###############################################################################

#### EndOfFile
