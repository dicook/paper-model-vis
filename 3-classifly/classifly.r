library(ggplot2)
library(classifly)
library(reshape)
library(MASS)
library(randomForest)
library(e1071)

# source("../1-data/wine.r")
wine <- read.csv("../1-data/wine.csv")
winer <- rescaler(wine, "range")

head(wine); head(winer)

qplot(color, phenols, data=wine, colour=type) 

wine_lda <- classifly(winer, type ~ color + phenols, lda, n = 100 * 100, method="grid")
grid <- subset(wine_lda, .TYPE == "simulated")

ggplot(grid, aes(color, phenols)) + geom_tile(aes(fill = type), colour=NA, alpha=0.3) + 
  geom_point(aes(colour=type), data=winer) + coord_equal()
ggsave(file = "class-2d-shade.pdf", width=5, height=4)

ggplot(grid, aes(color, phenols, colour=type)) + geom_point(data=winer) + 
  geom_contour(data = grid, aes(x=color, y=phenols, z=.ADVANTAGE, colour=type), 
               size = 0.5, breaks=0.05) + coord_equal() #+ scale_z_continuous(breaks=0.05) 
ggsave(file = "class-2d-boundary.pdf", width=5, height=4)


w <- read.csv("../6-nnet/wiggly.csv")[, -1]
w$class <- factor(w$class)
head(w)
w_rf <- classifly(w, class ~ x + y, randomForest)

#winefly(lda)
wine_lda <- classifly(winer, type ~ color + phenols + flavanoids, lda)
#winefly(svm, probability=T, kernel="linear")
wine_svml <- classifly(winer, type ~ color + phenols + flavanoids, svm, kernel="linear", probability=T)

f5 <- type ~ color + phenols + flavanoids + proline + dilution
#winefly(svm, probability=T, kernel="polynomial", formula = f5, n=1e6)
wine_svmp <- classifly(winer, f5, svm, n=200*200, kernel="polynomial", probability=T)

write.csv(wine_svmp, file="wine-svm-poly.csv", row.names=F)

#winefly(svm, probability=T, kernel="radial")
wine_svmr <- classifly(winer, type ~ color + phenols + flavanoids, svm, n=500*500, kernel="radial", probability=T)
str(wine_svmr)

write.csv(wine_svmr, file="wine-svm-radial.csv", row.names=F)

# Random forests don't give clean boundaries
