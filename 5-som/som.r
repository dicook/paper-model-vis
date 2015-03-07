# source("~/documents/clusters/clusterfly/load.r")
library(clusterfly)
library(kohonen)
library(RGtk2)
source("som-iterate.r")
source("../1-data/wine.r")

# wgobi(wine)
vars <- c("dilution", "alcohol", "color", "proline", "flavanoids")
w5 <- as.matrix(winer[, vars])
init <- cbind(expand.grid(seq(min(w5[,1]), max(w5[,1]), length.out=10), seq(min(w5[,2]), max(w5[,2]), length.out=3)), matrix(rep(0,30*3), ncol=3))
init.grid<-matrix(c(1:9, 11:19, 21:29, 1:10, 11:20, 2:10, 12:20, 22:30, 11:20, 21:30), ncol=2)
#init <- cbind(expand.grid(seq(min(w5[,1]), max(w5[,1]), length.out=3), seq(min(w5[,2]), max(w5[,2]), length.out=3)), matrix(rep(0,9*3), ncol=3))
#init.grid<-matrix(c(1:2, 4:5, 7:8, 1:3, 4:6, 2:3, 5:6, 8:9, 4:6, 7:9), ncol=2)
#init <- cbind(expand.grid(seq(min(w5[,1]), max(w5[,1]), length.out=4), seq(min(w5[,2]), max(w5[,2]), length.out=4)), matrix(rep(0,16*3), ncol=3))
#init.grid<-matrix(c(1:3, 5:7, 9:11, 13:15, 1:4, 5:8, 9:12, 2:4, 6:8, 10:12, 14:16, 5:8, 9:12, 13:16), ncol=2)
colnames(init)<-vars

# Visualising iterations -----------------------------------------------------

right <- som_iterate(w5, grid = somgrid(10, 3), radius=c(3, 0), stepsize=1, nsteps=100, init=init)
#right <- som_iterate(w5, grid = somgrid(3, 3), radius=c(3, 0), stepsize=1, nsteps=100, init=init)
ggobi.som(right, id=1, cls=wine$type, map=init.grid)
ggobi.somiter(right, cls=wine$type, map=init.grid)

distances <- melt(sapply(right, function(x) x$distances))
names(distances) <- c("oid", "step", "value")

right_sum <- summary(right)[, -c(7,8)]

ggplot(right_sum, aes(step, dist_mean)) + 
  geom_line(aes(y=value, group = oid), colour = alpha("grey10", 0.2),
    data = distances) + 
  geom_line(colour="black") + 
  geom_point(aes(colour = radius), data = subset(right_sum, step %% 2 == 0)) + 
  scale_y_continuous("distance from node") + 
  scale_colour_gradient(breaks = 1:3, high = "yellow")

ggsave("som-dist.pdf", width=10, height=4) 

ggplot(right_sum, aes(step, dist_mean)) + 
  geom_line(colour="black") + 
  geom_point(aes(colour = radius), data = subset(right_sum, step %% 2 == 0)) + 
  scale_y_continuous("distance from node") + 
  scale_colour_gradient(breaks = 1:3, high = "yellow")

ggsave("som-dist-mean.pdf", width=10, height=3) 

final <- right[[length(right)]]

# m-in-ds vs d-in-ms ---------------------------------------------------------

initial <- right[[1]]

pdf("wine-bad-model.pdf", width = 6, height = 3)
plot(initial)
dev.off()

wgobi(initial)
