library(ggplot2)
library(nnet)
library(plyr)
theme_update(panel.background = element_rect(fill = "grey95", colour = NA))

w <- read.csv("6-nnet/wiggly.csv")[ ,2:4]
w <- transform(w, class = factor(class, labels = c("A", "B")))

sqr <- theme(aspect.ratio = 1)
nolab <- list(scale_x_continuous(""), scale_y_continuous(""))
col_d <- scale_colour_manual(values = c("#377EB8", "#E41A1C"))
fill_c <- scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "#377EB8",
  midpoint = 0.5)
# fill_c$train(c(0, 1))

source("6-nnet/explore.r")
(load("6-nnet/wiggly-multi.rdata"))
many <- rbind(many2$coef, many3$coef, many4$coef)
many <- plyr::rename(many, c(size = "nodes"))
many$id <- (many$nodes - 2) * 200 + as.integer(many$i)

# Plot sample data -----------------------------------------------------------

qplot(x, y, data = w, colour = class, shape = class, xlab = "", ylab = "") +
  sqr +
  col_d +
  nolab
ggsave("6-nnet/nnet-test.pdf", width = 6, height = 5)

# Display model in data space ------------------------------------------------

best <- Filter(function(x) accuracy(x) == 1, many4$all)[[1]]

ggplot(subset(best$output,  node == 1), aes(x, y)) +
  geom_raster(aes(fill = pred)) +
  geom_point(aes(shape = class), data = w) +
  sqr +
  fill_c +
  nolab
ggsave("6-nnet/nnet-best-fill.pdf", width = 6, height = 5)

ggplot(subset(best$output, node == 1), aes(x, y)) +
  geom_point(aes(shape = class, colour = class), data = w) +
  sqr +
  col_d +
  geom_contour(aes(z = pred), breaks = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  nolab
ggsave("6-nnet/nnet-best-contour.pdf", width = 6, height = 5)

# Display hidden nodes -------------------------------------------------------

hidden_fill <- scale_fill_gradient2(low = "#AF8DC3", mid = "#F7F7F7",
  high = "#7FBF7B", midpoint = 0.5, limits = c(0, 1))

ggplot(best$hidden, aes(x, y)) +
  geom_raster(aes(fill = pred)) +
  geom_point(aes(shape = class), data = w) +
  sqr +
  facet_grid(. ~ node) +
  hidden_fill +
  nolab
ggsave("6-nnet/nnet-hidden-fill.pdf", width = 12, height = 3)

ggplot(best$hidden, aes(x, y)) +
  geom_contour(aes(z = pred, group = node), colour = "grey50", size = 2,
    breaks = 0.5) +
  geom_point(aes(colour = class, shape = class), data = w) +
  sqr + col_d + nolab
ggsave("6-nnet/nnet-hidden-contour.pdf", width = 6, height = 5)

# Compare fits with different numbers of nodes -------------------------------

qual <- unique(many[, c("value", "accuracy", "nodes", "id")])
qplot(accuracy, value, data = qual, xlab = "",
  ylab = "Value of fitting criterion", facets = . ~ nodes)
ggsave("6-nnet/nnet-value-accuracy.pdf", width = 8, height = 3)

qplot(accuracy, data = qual, geom = "histogram", facets = . ~ nodes,
  binwidth = 0.01, xlab = "Prediction accuracy")
ggsave("6-nnet/nnet-accuracy.pdf", width = 8, height = 3)

# Display all fits on a single plot ------------------------------------------

all <- mapply(function(nn, i, j) {
  df <- subset(nn$output, node==1)
  df$id <- i
  df$nodes <- j
  df
}, c(many2$all,many3$all,many4$all), 1:600, rep(2:4, each=200), SIMPLIFY = FALSE)
all <- do.call("rbind", all)

source("6-nnet/contour.r")
contours <- ddply(all, .(nodes, id), function(df) contour(df$x, df$y, df$pred))
contours <- subset(contours, piece == 1)
contours <- merge(contours, qual)
contours$acc <- cut(contours$accuracy, c(0, 0.915, 0.925, 0.98, 1),
  labels = c("Sub-linear", "Linear", "Super-linear", "Excellent"))

ggplot(contours, aes(x, y)) + sqr +
  geom_path(aes(group = id), colour = scales::alpha("grey50", 0.5)) +
  facet_grid(nodes ~ acc) +
  nolab
ggsave(file = "nnet-all.png", width=8, height=6)

return()

# Animation of fit over time -------------------------------------------------

load("iterations.rda")
# results <- vector("list", 100)
#
# results[[1]] <- gen_preds(w[,-3], w[,3], maxit = 1, size=4)
# for(i in 2:100) {
#   print(i)
#   results[[i]] <- gen_preds(w[,-3], w[,3], maxit = 5, Wts=results[[i - 1]]$nn$wts, size=4)
# }

hidden_seq <- mapply(function(nn, i) transform(nn$hidden, time = i), results, seq_along(results), SIMPLIFY = FALSE)
hidden_seq <- do.call("rbind", hidden_seq)

# Static shots

ggplot(data=hidden_seq, aes(x, y)) +
  geom_contour(aes(z=pred, group=time, colour=time), breaks = 0.5) +
  sqr +
  facet_grid(. ~ node) +
  scale_colour_continuous(low="#FFF7BC", high="#D95F0E") + nolab

ggsave(file = "nnet-iteration.png", width=12, height=3)

qplot(x, y, data=subset(hidden_seq, time < 4), geom="contour", z=pred, colour=factor(node), group=interaction(node, time), breaks = 0.5) + facet_grid(. ~ time)

# Movie -----------------------
grad <- scale_fill_gradient2(low="#AF8DC3", mid="#F7F7F7", high="#7FBF7B", midpoint=0.5)
grad$train(c(0, 1))

surface <- ggplot(data=best$hidden, aes(x, y)) + geom_tile(aes(fill=pred)) + geom_point(aes(shape=class), data=w) +sqr + facet_grid(. ~ node) + grad

source("~/documents/plyr/plyr/load.r")
d_ply(hidden_seq, u(time), function(df) {
  path <- ps("movie/time-", sprintf("%03d", df$time[1]), ".pdf")
  ggsave(surface %+% df, file = path, height = 4, width = 4 * 4)
})

# Interactive contour lines --------------------------------------------------

# generate 50% contour line from all$output, labelled by id
#  (many2$all[[i]]$output), add with ggobi_longitudinal
# link with qual dataset
