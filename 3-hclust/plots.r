library(DescribeDisplay)

d <- dd_load("3-hclust/wine-hcward-1.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 18] <- 0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 16] <- 0.3
d$plots[[1]]$points$col[d$plots[[1]]$points$pch == 18] <- "#808080"

pdf("3-hclust/wine-hcward-1.pdf", width = 5.5, height = 2)
plot(d)
dev.off()

d <- dd_load("3-hclust/wine-hcward-2.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 18] <- 0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 16] <- 0.5
d$plots[[1]]$points$col[d$plots[[1]]$points$pch == 18] <- "#808080"

pdf("3-hclust/wine-hcward-2.pdf", width = 3, height = 3)
plot(d, axislocation = c(0.2, 0.15))
dev.off()

d <- dd_load("3-hclust/wine-hcward-3.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 18] <- 0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch == 16] <- 0.5
d$plots[[1]]$points$col[d$plots[[1]]$points$pch == 18] <- "#808080"

pdf("3-hclust/wine-hcward-3.pdf", width = 3, height = 3)
plot(d, axislocation = c(0.2, 0.15))
dev.off()
