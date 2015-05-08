library(plyr)
library(reshape2)
library(ggplot2)

source("1-data/wine.r")
source("5-projection-pursuit/anneal.r")

# Perform simulated annealing ------------------------------------------------

winem <- as.matrix(winer[, -1])
# trace <- replicate(20, scagnostics.anneal(winem, 3, max.steps = 40), simplify=FALSE)
# for(i in 1:length(trace)) trace[[i]]$trace <- i
# trace_df <- do.call("rbind", trace)
# best <- trace_df[order(trace_df$index, decreasing=TRUE)[1:4], ]
# save(trace, trace_df, best, file="trace.rda", compress=TRUE)
load("5-projection-pursuit/trace.rda")

# Plot paths -----------------------------------------------------------------

ggplot(trace_df, aes(step, index, group = trace)) +
  geom_line() +
  geom_point(size = 1) +
  ylim(c(0, NA))
ggsave("5-projection-pursuit/pp-index-progression.pdf", width = 8, height = 4)

# Plot best projections ------------------------------------------------------

plot_proj <- function(i) {
  bproj <- as.data.frame(winem %*% best$proj[[i]])
  bproj$type <- winer$type

  ggplot(bproj, aes(pp1, pp2, colour = type)) +
    geom_point() +
    xlab(NULL) +
    ylab(NULL) +
    col +
    theme(legend.position = "none")
}

best_proj <- lapply(1:4, plot_proj)
paths <- paste0("5-projection-pursuit/proj-", 1:4, ".pdf")
invisible(mapply(ggsave, paths, best_proj, MoreArgs = list(width = 3, height = 3)))

# Restart from local optima --------------------------------------------------

# second <- best[2, ]
# trace12 <- subset(trace_df, trace == second$trace)
# restart <- rlply(5,
#   scagnostics.anneal(winem, 3, max.steps = 10, start=best$proj[[2]], temp=0.4),
# )
# save(restart, file="restart.rda", compress=TRUE)
load("5-projection-pursuit/restart.rda")

for (i in 1:length(restart)) restart[[i]]$trace <- i
restart_df <- plyr::rbind.fill(restart)
restart_df$step <- restart_df$step + second$step - 1

ggplot(restart_df, aes(step, index, group = trace)) +
  geom_line() +
  geom_line(data = trace12, colour = "red")
ggsave("5-projection-pursuit/pp-restart.pdf", width = 8, height = 4)

# Projection paths -----------------------------------------------------------

proj_to_df <- function(proj) {
  rownames(proj) <- names(winer[, -1])
  proj_df <- as.data.frame(proj)
  proj_df$id <- rownames(proj_df)
  projm <- melt(proj_df, id = "id")
  names(projm) <- c("variable", "axis", "value")
  projm
}
projections <- ddply(trace_df, .(trace, step, try), function(df) {
  cbind(proj_to_df(df$projs[[1]]), index = df$index)
})

# projections$qual <- chop(projections$index)
projections$variable <- factor(projections$variable)

# qplot(variable, value, data=projections, facets = axis ~ qual, geom="blank") + geom_line(aes(group=interaction(step, trace)), colour=alpha("black", 0.2))

# qplot(reorder(variable, value), value, data=subset(projections, index > 0.3), facets = axis ~ qual, geom="blank") + geom_line(aes(group=interaction(step, trace)))

qplot(index, data = trace_df, binwidth = 0.01)
pd <- dcast(projections, ... ~ variable + axis)

pdist <- dist(pd[, -(1:5)]) ^ 2
ord <- MASS::isoMDS(pdist, k = 4)

d2 <- data.frame(ord$points, pd[,1:5])
qplot(X1, X2, data = d2, colour = index)
