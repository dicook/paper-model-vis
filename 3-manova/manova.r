library(MASS)
library(ggplot2)
library(plyr)

source("../1-data/wine.r")

# Example data ---------------------------------------------------------------

df <- as.data.frame(rbind(
  mvrnorm(100, mu = c(1, 1), Sigma = matrix(c(1, -0.9, -0.9, 1), nrow = 2)),
  mvrnorm(100, mu = c(0, 0), Sigma = matrix(c(1, -0.9, -0.9, 1), nrow = 2))
))

col <- scale_colour_manual("group", values = c("#AF8DC3", "#7FBF7B"))

qplot(V1, V2, data = df, colour = rep(c("A","B"), each = 100)) + col
ggsave("manova-example.pdf", width = 5, height = 4)

qplot(V1, data = df, colour = rep(c("A", "B"), each = 100), geom = "density") + col
ggsave("manova-example-m1.pdf", width = 5, height = 4)

qplot(V2, data = df, colour = rep(c("A","B"), each = 100), geom = "density") + col
ggsave("manova-example-m2.pdf", width = 5, height = 4)

# Wine data ------------------------------------------------------------------

centre <- function(x) x - mean(x)
winec <- ddply(winer, .(type), numcolwise(centre))
wine_var <- var(winec[, -1])

source("3-manova/ellipse.r")
ellipses <- ddply(winer, .(type), function(df) conf.ellipse(df[, -1], cov=wine_var, npoints=10000))

data <- lattice::make.groups(regions = ellipses, data = winer)

g <- ggobi(data)
glyph_colour(g[[1]]) <- c(1, 5, 6)[data$type]
glyph_type(g[[1]]) <- c(6, 3)[data$which]
glyph_size(g[[1]]) <- c(1, 3)[data$which]
