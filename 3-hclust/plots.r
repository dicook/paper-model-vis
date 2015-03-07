library(DescribeDisplay)

d<-dd_load("wine-hcward-1.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==18]<-0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==16]<-0.3
d$plots[[1]]$points$col[d$plots[[1]]$points$pch==18]<-"#808080"
plot(d)

d<-dd_load("wine-hcward-2.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==18]<-0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==16]<-0.5
d$plots[[1]]$points$col[d$plots[[1]]$points$pch==18]<-"#808080"
plot(d, axislocation=c(0.85,0.15))

d<-dd_load("wine-hcward-3.r")
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==18]<-0.01
d$plots[[1]]$points$cex[d$plots[[1]]$points$pch==16]<-0.5
d$plots[[1]]$points$col[d$plots[[1]]$points$pch==18]<-"#808080"
plot(d, axislocation=c(0.15,0.15))
