library(ggplot2)
library(kohonen)
library(meifly)
data(nir)
str(nir)
dim(nir$spectra)
dim(nir$composition)
length(nir$temperature)

nir.spectra <- data.frame(nir$spectra)
nir.spectra$ethanol <- nir$composition[,1]
nir.spectra$water <- nir$composition[,2]
nir.spectra$isopropanol <- nir$composition[,3]
nir.spectra$temperature <- nir$temperature
nir.spectra$training <- nir$training

nir.spectra$id <- 1:nrow(nir.spectra)
nir.melt <- melt(nir.spectra, measure.var=1:200, id = c("id", "temperature", "ethanol"))
nir.melt$variable <- as.numeric(as.character(gsub("X","", nir.melt$variable)))

qplot(variable, value, data = nir.melt, group = id, geom = "line", colour = ethanol, xlab="Wavelength Number",ylab="Transmittance",main="Ethanol") + scale_colour_continuous(low="blue",high="yellow") + opts(legend.position="none")
ggsave("4-meifly/spectrum-ethanol2.png", width=5, height=7)

nir.spectra$id <- 1:nrow(nir.spectra)
nir.melt <- melt(nir.spectra, measure.var=1:200, id = c("id", "temperature", "isopropanol"))
nir.melt$variable <- as.numeric(as.character(gsub("X","", nir.melt$variable)))

qplot(variable, value, data = nir.melt, group = id, geom = "line", colour = isopropanol, xlab="Wavelength Number",ylab="Transmittance",main="Isopropanol") + scale_colour_continuous(low="blue",high="yellow") + opts(legend.position="none")
ggsave("4-meifly/spectrum-isoprop2.png", width=5, height=7)

nir.spectra$id <- 1:nrow(nir.spectra)
nir.melt <- melt(nir.spectra, measure.var=1:200, id = c("id", "temperature", "water"))
nir.melt$variable <- as.numeric(as.character(gsub("X","", nir.melt$variable)))

qplot(variable, value, data = nir.melt, group = id, geom = "line", colour = water, xlab="Wavelength Number",ylab="Transmittance",main="Water") + scale_colour_continuous(low="blue",high="yellow") + opts(legend.position="none")
ggsave("4-meifly/spectrum-water2.png", width=5, height=7) 

if (file.exists("models-50k.rdata")) {
  load("models-50k.rdata")
} else {
  # Sample variables and generate model formulae
  set.seed(12345)
  vars <- raply(50000, sort(sample(1:200, 6)))
  vars <- unique(vars)
	
  var_names <- names(nir.spectra)[vars]
  dim(var_names) <- dim(vars)
  formulas <- paste("ethanol ~ temperature + I(temperature^2) + ",
    aaply(var_names, 1, paste, collapse = " + "))

  # Fit models
  train <- nir.spectra[nir.spectra$training, ] 
  models <- llply(formulas, function(f) lm(as.formula(f), data = train, 
    model = FALSE), .progress = "text")
  names(models) <- aaply(vars, 1, paste, collapse = "-")
  class(models) <- "ensemble"

  save(vars, models, file = "models-50k.rdata")
}


fit <- summary(models[[1]])

fit.melt <- melt(fit, id.vars=c("model", "df"))
qplot(df, value, data=fit.melt)+facet_wrap(facets=~variable, ncol=5, scales ="free_y") + xlab("Degrees of Freedom")

fit$df
fit$r.squared
fit$adj.r.squared
