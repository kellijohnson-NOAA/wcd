#' ----
#' Title: Partitioning analysis of catches by gear type
#' Author: Kelli Faye Johnson
#' ----

#' Subset for federal period
props <- props[props$year >= federalyear, ]
fed <- catch[catch$year >= federalyear, ]

#' Calculate the distance between each observation
res <- vegdist(props, method = "bray", binary = FALSE, diag = FALSE,
  upper = FALSE)
mod <- hclust(res, method = "complete")

#' Determine the optimal number of partitions
tryk <- 2:(NROW(props) - 1)
groups <- sapply(tryk, function(x) {
  summary(cluster::silhouette(cutree(mod, x), res))$avg.width
})
k <- which.max(groups) + 1
clust <- cutree(mod, k = k)
clust.df <- data.frame(label = rownames(props), cluster = factor(clust))

dendr <- dendro_data(mod)
dendr$labels <- merge(dendr$"labels", clust.df, by = "label")
rect <- aggregate(x ~ cluster, dendr$labels, range)
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
  ggplot() +
    geom_segment(data = data.frame(tryk, groups),
      aes(x = tryk, xend = tryk, y = -Inf, yend = groups)) +
    xlab("number of groups") +
    ylab("average silhouette width") +
    geom_point(data = data.frame(k, groups[k - 1]),
      aes(x = k, y = groups[k - 1])) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  # fviz_nbclust(props, hcut, method = "wss", linecolor = "black") +
    theme +
    ggtitle("") +
    theme(axis.title.x = element_text(size = rel(0.8)),
      axis.title.y = element_text(size = rel(0.8)),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA))))

#' Set up the page
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
# Make each plot, in the correct location
for (i in seq_along(plots)) {
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
    layout.pos.col = matchidx$col))
}
dev.off()

#' Simpson Diversity Index
simp <- diversity(catch[, c("HKL", "POT", "TWL")],
  index = "simpson", MARGIN = 1)

png(file.path(dir.results, "simpsonsdiversity.png"),
  res = resolution, width = width, height = height / 1.2)
plot(simp, xaxt = "n", xlab = "year", ylab = "evenness", las = 1,
  yaxs = "i", xaxs = "i", type = "b")
abline(h = mean(simp), lty = 2)
text(x = 1, y = mean(simp)*0.98, label = "mean evenness", pos = 4)
mean <- mean(simp[catch$year >= federalyear])
abline(h = mean, lty = 2)
abline(v = which(rownames(catch) == federalyear), lty = 2)
text(x = 1, y = mean * 0.98, label = "mean evenness since 1982", pos = 4)
seq <- seq(1, NROW(catch), by = 4)
axis(1, at = (1:NROW(catch))[seq], labels = rownames(catch)[seq], las = 2)
below <- match(names(which(simp[catch$year >= federalyear] < mean)),
  rownames(catch))
points(below, rep( par("usr")[3], length(below)), pch = 19,
  col = "black", xpd = TRUE)
dev.off()

#' Multidimensional scaling analysis
nmds <- metaMDS(props[, c("HKL", "POT", "TWL")], k = 2,
  wascores = TRUE, autotransform = FALSE)

png(file.path(dir.results, "nmds.png"),
  res = resolution, width = width, height = height)
ordiplot(nmds, type = "n", las = 1)
orditorp(nmds, display = "species", col = "black", air = 0.01, cex = 1.5)
orditorp(nmds, display = "sites", col = "black", air = 0.01)
abline(v = 0, h = 0, lty = 2)
dev.off()
