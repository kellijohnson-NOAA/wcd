#' ----
#' Title: Partitioning analysis of catches by gear type
#' Author: Kelli Faye Johnson
#' ----

#' Read in the catch data as provided for the stock assessment
#' Catches are grouped according to gear that was used while fishing
#' not permit type held while fishing.
catch <- read.csv(file.path(dir.data, "catchbyyearfleet.csv"))
catch <- aggregate(catch ~ year + fleet, data = catch, sum)
catch <- reshape(catch, direction = "wide", timevar = "fleet", idvar = "year")

#' Change absolute catches into proportion of the total catch for each gear
props <- data.frame("year" = catch[, 1],
  prop.table(as.matrix(catch[, -1]), margin = 1))
props <- subset(props, year >= 1982)

#' Calculate the distance between each observation
res <- dist(props)
mod <- hclust(res)

clust <- cutree(mod, h = 15)
clust.df <- data.frame(label = rownames(props), cluster = factor(clust))
k <- 3
dendr <- dendro_data(mod, type = "rectangle")
dendr$labels <- merge(dendr$"labels", clust.df, by = "label")
rect <- aggregate(x ~ cluster, label(dendr), range)
rect <- data.frame(rect$cluster, rect$x)
ymax <- mean(mod$height[length(mod$height) - ((k - 2):(k - 1))])

#' Create the figure used in the publication
png(file.path(dir.results, "dendrogram.png"),
  res = resolution, width = width, height = height / 1.2)
layout <- matrix(c(1, 1, 1, 2, 1, 1, 2, 1,  1), ncol = 3)
plots <- c(list(
  ggplot(segment(dendro_data(as.dendrogram(mod), type = "rectangle"))) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme +
    xlab("") +
    ylab("dissimilarity") +
    scale_x_discrete(breaks = seq(1, NROW(props), by = 1),
      labels = props$year[mod$order]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_rect(data = rect, aes(xmin = X1 - 0.3,
      xmax = X2 + 0.3, ymin = 0, ymax = ymax),
      color = "black", fill = NA, lty = 2),
  fviz_nbclust(props, hcut, method = "wss") +
    theme +
    geom_vline(xintercept = 3, linetype = 2) +
    ylab("Total w/i SS") +
    ggtitle("") +
    theme(axis.title.x = element_text(size = rel(0.8)),
      axis.title.y = element_text(size = rel(0.8)),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA))))

# Set up the page
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
# Make each plot, in the correct location
for (i in seq_along(plots)) {
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
    layout.pos.col = matchidx$col))
}
dev.off()
