library(ggplot2)
library(rggobi)

theme_update(panel.background = element_rect(fill = "grey95", colour = NA))

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescaler <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  df
}

# Load wine data and create version rescaled to 0-1
wine <- read.csv("1-data/wine.csv")
winer <- rescaler(wine)

wgobi <- function(...) {
  g <- ggobi(...)
  glyph_colour(g[[1]]) <- c(1, 5, 6)[wine$type]
  invisible(g)
}

col <- scale_colour_manual("Variety", values = c("#984EA3", "#4DAF4A", "#FF7F00"))
fill <- scale_fill_manual("Variety", values = scales::alpha(c("#984EA3", "#4DAF4A", "#FF7F00"), 0.3))

recolour <- function(g = ggobi_get()[[1]]) {
  glyph_colour(g) <- c(1, 5, 6)[g$type]  #glyph_colour(g) + 1
}
