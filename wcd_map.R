#' ----
#' Title: Spatial data regarding US West Coast groundfish fisheries
#' Author: Kelli Faye Johnson
#' ----
#+ setup, include = FALSE
# set global chunk options
opts_chunk$set(fig.path = file.path(dir.results, "figure"),
  fig.align = "center", fig.show = "hold")
options(formatR.arrow = TRUE, width = 90)

#' Goal - How the fishery responds to multiple socio-ecological drivers?
#'
#' Species -
#' * cowcod - not included b/c minimal to zero bycatch
#' * Pacific Ocean Perch (POP) - rebuilt but included b/c high bycatch
#'
#'Raw landed weight of sablefish and petrale from the Northwest Fisheries
#'Science Center Shelf-Slope Survey. The size of the circle is proportional to the
#'landed weight, with circles being semi-transparent to show tows within close
#'proximity of each other.

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
us <- getData("GADM", country = "USA", level = 1)
canada <- getData("GADM", country = "CAN",level = 1)

us.states <- us[us$NAME_1 %in% c("California", "Oregon", "Washington"), ]
ca.provinces <- canada[canada$NAME_1 %in% c("British Columbia"), ]

us.bbox <- bbox(us.states)
xlim <- c(min(us.bbox[1, 1]) * 1.02, max(us.bbox[1, 2]))
ylim <- c(min(us.bbox[2, 1]) * 0.98, max(us.bbox[2, 2]))

# p <- ggmap(get_map(location = "Redding, California", source = "google",
#   maptype = "roadmap", zoom = 5, messaging = FALSE, color = "color"))

p <- ggplot() +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7, face = "bold")
  ) +
  xlim(xlim) + ylim(ylim) + xlab("") + ylab("")

small <- 0.10
p +
  geom_point(data = data.plot,
    aes(x = X, y = Y, size = weight / 10), alpha = 0.25) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6)) +
  geom_path(data = us.states, aes(x = long, y = lat, group = group), size = small) +
  geom_path(data = ca.provinces, aes(x = long, y = lat, group = group), size = small) +
  coord_map() +
  facet_grid(species ~ year)
ggsave(filename = "abundance.png",
  plot = last_plot(), path = dir.results, scale = 1,
  width = par("din")[1], height = par("din")[2], units = "in",
  dpi = 300, limitsize = TRUE)

getstrata <- eval(parse(text = gsub(",$", "",
  grep("SLat", readLines("wcd_survey.R"), value = TRUE))))
p +
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
ggsave(filename = "strata.png",
  plot = last_plot(), path = dir.results, scale = 1,
  width = par("din")[1], height = par("din")[2], units = "in",
  dpi = 300, limitsize = TRUE)
dev.off()

#+endoffile, echo = FALSE
setwd(dir.old)
rm(dir.old)
