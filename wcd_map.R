#' ----
#' Title: Spatial data regarding US West Coast groundfish fisheries
#' Author: Kelli Faye Johnson
#' ----

# Variable inputs
small <- 0.10

#### Data
getstrata <- eval(parse(text = gsub(",$", "",
  grep("SLat", readLines("wcd_survey.R"), value = TRUE))))
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

#### ggplot theme
theme <-   theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7, face = "bold")
  )

p <- ggplot() +
  theme +
  xlim(xlim) + ylim(ylim) + xlab("") + ylab("")

#### Start individual maps
###############################################################################
pp <- p +
  geom_point(data = data.plot,
    aes(x = X, y = Y, size = weight / 10), pch = 1, alpha = 0.5) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6)) +
  geom_path(data = us.states, aes(x = long, y = lat, group = group), size = small) +
  geom_path(data = ca.provinces, aes(x = long, y = lat, group = group), size = small) +
  coord_map() +
  facet_wrap( ~ species) +
  theme(strip.text = element_text(vjust = -1.25),
        panel.margin = unit(c(-0.5, 0, -0.5, 0), "lines"))
ggsave(filename = "abundance.png", pp, path = dir.results,
  dpi = 300, limitsize = TRUE)
###############################################################################

###############################################################################
pp <- p +
  theme(legend.position = "none",
        axis.text = element_text(size = 6)) +
  geom_path(data = us.states, aes(x = long, y = lat, group = group), size = small) +
  geom_path(data = ca.provinces, aes(x = long, y = lat, group = group), size = small) +
  coord_map() +
  geom_hline(yintercept = getstrata[-length(getstrata)], lty = 2) +
  annotate("text", x = -122.2, y = 40.433, label = "Cape Mendocino", size = 3) +
  annotate("text", x = -122.9, y = 42.833, label = "Cape Blanco", size = 3) +
  annotate("text", x = -125.5, y = getstrata + 0.5,
           label = paste0("(", letters[1:9], ")"), size = 3)
ggsave(filename = "strata.png", pp, path = dir.results, scale = 1,
  width = par("din")[1], height = par("din")[2], units = "in",
  dpi = 300, limitsize = TRUE)
dev.off()
###############################################################################

###############################################################################
indexlong <- reshape(data.yrin,
  direction = "long", varying = my.spp, v.name = "weight",
  timevar = "species", times = my.spp)
rownames(indexlong) <- NULL
indexlong$species <- sapply(strsplit(indexlong$species, "\\."), "[", 1)
indexlong$species[indexlong$species == "Pacific"] <- "POP"
# Print correct levels for letters
levels(data.cost$portgrp)[c(9, 1, 7, 3, 2, 4, 5, 8, 6)]

pp <- ggplot() + theme +
  geom_line(data = subset(indexlong, year %in% 2009:2013),
    aes(x = year, y = weight)) +
  facet_grid(species ~ strat, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))
ggsave(filename = "indexlines.png", pp, path = dir.results, scale = 1,
  dpi = 300, limitsize = TRUE)
dev.off()
###############################################################################

#### endoffile
setwd(dir.old)
rm(dir.old)
