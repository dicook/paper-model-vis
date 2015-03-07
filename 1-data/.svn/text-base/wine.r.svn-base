library(ggplot2)
ggopt(grid.fill = "grey95")
source("~/documents/reshape/reshape/load.r")

wine <- read.csv("../1-data/wine.csv")
winer <- rescaler(wine, "range")

library(rggobi)
wgobi <- function(...) {
  g <- ggobi(...)
  glyph_colour(g[[1]]) <- c(1, 5, 6)[wine$type]  
  invisible(g)
}
wgobi(wine)

col <- scale_colour_manual("Variety", values = c("#984EA3", "#4DAF4A", "#FF7F00"))
fill <- scale_fill_manual("Variety", values = alpha(c("#984EA3", "#4DAF4A", "#FF7F00"), 0.3))

recolour <- function(g = ggobi_get()[[1]]) {
  glyph_colour(g) <- c(1, 5, 6)[g$type]  #glyph_colour(g) + 1  
}
winefly <- function(..., formula = type ~ color + phenols + flavanoids, n=1e5) {
  print(classifly(wine, formula, ..., n=1e5))
  recolour()
}

