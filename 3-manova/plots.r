library(DescribeDisplay)

d1 <- dd_load("3-manova/manova-1.r")
d2 <- dd_load("3-manova/manova-2.r")

png("3-manova/manova-1.png", width = 5, height = 5, units = "in", res = 150)
plot(d1, axislocation = c(0.2, 0.15))
dev.off()

png("3-manova/manova-2.png", width = 5, height = 5, units = "in", res = 150)
plot(d2, axislocation = c(0.2, 0.15))
dev.off()
