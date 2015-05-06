library(clusterfly)
source("1-data/wine.r")

# ggplot2 dengrograms ----------------------------------------------------------

gdendro <- function(ward2) {
  wardd <- ward2$data
  wardd$type <- factor(c(as.character(wine$type), rep(NA, nrow(wine) - 1)))
  vars <- c("ORDER", "POINTS", "type")

  leaves <- subset(wardd, node == 0)
  nodes <- subset(wardd, node == 1)

  # < 0 = observation, > 0 = cluster
  merge <- ward2$hclust$merge
  merge[merge > 0] <- merge[merge > 0] + nrow(leaves)
  merge[merge < 0] <- abs(merge[merge < 0])

  lines <- data.frame(rbind(
    cbind(nodes[, vars], wardd[merge[, 1], vars]),
    cbind(nodes[, vars], wardd[merge[, 2], vars])
  ))

  ggplot(NULL, aes(ORDER, POINTS)) +
    geom_point(aes(colour = type), data = leaves) +
    geom_point(data = nodes, size = 0.5) +
    geom_segment(aes(xend = ORDER.1, yend = POINTS.1, group = 1), data = lines) +
    scale_x_continuous("",breaks = NULL) +
    scale_y_continuous("Number of points in cluster") +
    col
}

# Explore wine data -----------------------------------------------------------

sngl2 <- hierfly(winer[, -1], method = "single")
ward2 <- hierfly(winer[, -1], method = "ward.D")

source("3-hclust/dendro.r")
gdendro(ward2)
ggsave("3-hclust/wine-ward.pdf", width = 6, height = 4)
gdendro(sngl2)
ggsave("3-hclust/wine-single.pdf", width = 6, height = 4)

# g <- ggobi(sngl2)
g <- ggobi(ward2)
glyph_colour(g[[1]]) <- c(1, 5, 6)[wine$type]
# Open a tour of the variables, and a dendrogram using points vs order
