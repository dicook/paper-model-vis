library(ggplot2)
library(classifly)
library(MASS)
library(randomForest)
library(e1071)

source("1-data/wine.r")
winefly <- function(..., formula = type ~ color + phenols + flavanoids, n = 1e5) {
  print(classifly(wine, formula, ..., n = 1e5))
  recolour()
}

qplot(color, phenols, data = wine, colour = type) + col

wine_lda <- classifly(winer, type ~ color + phenols, lda, n = 250 * 250,
  method = "grid")
grid <- subset(wine_lda, .TYPE == "simulated")

ggplot(grid, aes(color, phenols)) +
  geom_raster(aes(fill = type)) +
  geom_point(aes(colour = type), data = winer) +
  col +
  fill +
  coord_equal()
ggsave(file = "3-classifly/class-2d-shade.pdf", width = 5, height = 4)

ggplot(grid, aes(color, phenols, colour = type)) +
  geom_point(data = winer) +
  geom_contour(data = grid, aes(z = .ADVANTAGE, colour = type),
    size = 1, breaks = 0.10) +
  col +
  coord_equal()
ggsave(file = "3-classifly/class-2d-boundary.pdf", width = 5, height = 4)

w <- rescaler(read.csv("6-nnet/wiggly.csv")[, -1])
w$class <- factor(w$class)
classifly(w, class ~ ., randomForest)

winefly(lda)
winefly(svm, probability = TRUE, kernel = "linear")

f5 <- type ~ color + phenols + flavanoids + proline + dilution
winefly(svm, probability = TRUE, kernel = "polynomial", formula = f5, n = 1e6)
winefly(svm, probability = TRUE, kernel = "radial")

# Random forests don't give clean boundaries
