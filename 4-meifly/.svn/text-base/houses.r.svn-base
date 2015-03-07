library(meifly)
library(rggobi)
library(ggplot2)

# NewHavenResidential
library(YaleToolkit)
data(NewHavenResidential)
head(NewHavenResidential)
y <- NewHavenResidential[,1]
x <- NewHavenResidential[,-c(1,3)]
models <- fitall(y, x, lm)
ggobi(models, NewHavenResidential)

# Model choice statistics
model.summary<-summary(models)
model.summary$df<-factor(model.summary$df)
x<-melt(rescaler(model.summary, "range"), id=c("model","df"))
x.max<-NULL
for (i in 3:9) {
  y<-subset(rescaler(model.summary, "range"), df==i)
  x.max<-rbind(x.max, c(i,apply(y[,2:6], 2, max)))
}
colnames(x.max)[1]<-"df"
x.max<-data.frame(x.max)
x.max$df<-factor(x.max$df)
y<-melt(x.max, id="df")
  
p <- qplot(df, value, data=x, xlab="Degrees of Freedom", ylab="Standardized Values")  + facet_wrap(~variable, ncol=5)
p <- p + geom_line(data=y, aes(x=as.numeric(df), y=value)) 
p
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-coef-models.pdf", width=12, height=3)

# Coefficients
model.coefs<-coefficients(models)
qplot(variable, raw, data=model.coefs)
qplot(variable, std, data=model.coefs, ylab="Standardized Estimates", xlab="Variable")
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-stdcoefs.pdf", width=8, height=5)

library(DescribeDisplay)
d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Variable",breaks=1:7, labels=c("livingArea","size","zoneRM","zoneRS","acTypeNo AC","bedrms","bathrms")) + scale_y_continuous("Standardized Estimates") + geom_rect(aes(xmin=5.9, xmax=6.1, ymin=0.2, ymax=0.5), fill=NA, size=I(4))
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing1.pdf", width=8, height=5)

d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Degrees of freedom") + scale_y_continuous(expression(R^2))
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing2.pdf", width=5, height=5)

d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Variable",breaks=1:7, labels=c("livingArea","size","zoneRM","zoneRS","acTypeNo AC","bedrms","bathrms")) + scale_y_continuous("Standardized Estimates") + geom_rect(aes(xmin=5.9, xmax=6.1, ymin=-0.45, ymax=-0.15), fill=NA, size=I(4))
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing3.pdf", width=8, height=5)

d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Degrees of freedom") + scale_y_continuous(expression(R^2))
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing4.pdf", width=5, height=5)

d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Residuals") + scale_y_continuous("Cooks D") + geom_rect(aes(xmin=-500000, xmax=-420000, ymin=1.05, ymax=1.15), fill=NA, size=I(1))
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing5.png", width=4, height=4)

d<-dd_load(file.choose())
ggplot(d) + scale_x_continuous("Size") + scale_y_continuous("Price") 
ggsave(file="/Users/dicook/ggobi-svn/papers/2009-model-vis/4-meifly/houses-brushing6.png", width=4, height=4)

