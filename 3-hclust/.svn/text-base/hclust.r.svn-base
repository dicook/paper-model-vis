library(reshape)
library(clusterfly)

source("../1-data/wine.r")


sngl2 <- hierfly(winer[, -1], method="single")
ward2 <- hierfly(winer[, -1], method="ward")

source("dendro.r")
gdendro(ward2)
ggsave(file = "wine-ward.pdf", width=6, height=4)
gdendro(sngl2)
ggsave(file = "wine-single.pdf", width=6, height=4)

g <- ggobi(sngl2)
g <- ggobi(ward2)
glyph_colour(g[[1]]) <- c(1, 5, 6)[wine$type]  
# Open a tour of the variables, and a dendrogram using points vs order
