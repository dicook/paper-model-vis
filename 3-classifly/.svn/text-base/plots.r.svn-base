library(stringr)
library(DescribeDisplay)

save_as_png <- function(path, ...) {
  d <- dd_load(path)
  out_path <- str_replace(path, "\\.r$", "\\.png")
  
  png(out_path, width = 5, height = 5, units = "in", res = 150)
  plot(d, ...)
  dev.off()
}

save_as_png("svm-radial-1.r", axislocation=c(0.15,0.15))
save_as_png("svm-radial-2.r", axislocation=c(0.15,0.15))
save_as_png("svm-radial-3.r", axislocation=c(0.15,0.15))
save_as_png("svm-radial-4.r", axislocation=c(0.15,0.15))
save_as_png("svm-radial-5.r", axislocation=c(0.15,0.15))
save_as_png("svm-radial-6.r", axislocation=c(0.15,0.15))

save_as_png("svm-poly1.r", axislocation=c(0.15,0.85))
save_as_png("svm-poly2.r", axislocation=c(0.15,0.85))
save_as_png("svm-poly3.r", axislocation=c(0.15,0.85))
save_as_png("svm-poly4.r", axislocation=c(0.15,0.85))
