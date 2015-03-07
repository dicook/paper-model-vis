setwd("/Users/dicook/ggobi-svn/papers/2009-model-vis")
load(".RData")     

library(DescribeDisplay)
d<-dd_load(file.choose())
d$plots[[1]]$points$cex <- rep(0.6, 208)
plot(d, axislocation=c(0.82, 0.88))

plot(init[,1], init[,2])
for (i in 1:47)
  lines(c(init[init.grid[i,1],1], init[init.grid[i,2],1]),
        c(init[init.grid[i,1],2], init[init.grid[i,2],2]))

plot(right[[11]])

gSignalHandlerDisconnect(g, 2262)
