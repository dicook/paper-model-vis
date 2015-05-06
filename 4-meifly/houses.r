library(meifly)
library(rggobi)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DescribeDisplay)

rescale01 <- function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))

# Fit all linear models to New Haven data --------------------------------------

data(NewHavenResidential, package = "barcode")

y <- NewHavenResidential[,1]
x <- NewHavenResidential[,-c(1,3)]
models <- fitall(y, x)
# ggobi(models, NewHavenResidential)

# Summary statistics -----------------------------------------------------------

model_sum <- models %>%
  summary() %>%
  tbl_df() %>%
  gather(statistic, val, logL:adjR2) %>%
  group_by(statistic) %>% mutate(stdval = rescale01(val)) %>%
  group_by(statistic, df) %>% mutate(rank = min_rank(desc(val))) %>%
  ungroup()

ggplot(model_sum, aes(df, stdval)) +
  geom_point() +
  geom_line(data = filter(model_sum, rank == 1)) +
  facet_wrap(~statistic, ncol = 5) +
  xlab("Degrees of Freedom") +
  ylab("Standardized Values")
ggsave("4-meifly/houses-coef-models.pdf", width = 12, height = 3)

# Coefficients
model_coef <- coefficients(models)
qplot(variable, std, data = model.coefs, ylab = "Standardized Estimates", xlab = "Variable")
ggsave("4-meifly/houses-stdcoefs.pdf", width = 8, height = 5)

vars <- c("livingArea", "size", "zoneRM", "zoneRS", "acTypeNo AC", "bedrms", "bathrms")

d <- dd_load("4-meifly/houses-brushing1.r")
ggplot(d) +
  scale_x_continuous("Variable", breaks = 1:7, labels = vars) +
  scale_y_continuous("Standardized Estimates") +
  annotate("rect", xmin = 5.9, xmax = 6.1, ymin = 0.2, ymax = 0.5, fill = NA, size = 2,
    colour = "black")
ggsave("4-meifly/houses-brushing1.pdf", width = 8, height = 5)

d <- dd_load("4-meifly/houses-brushing2.r")
ggplot(d) +
  scale_x_continuous("Degrees of freedom") +
  scale_y_continuous(expression(R ^ 2))
ggsave("4-meifly/houses-brushing2.pdf", width = 5, height = 5)

d <- dd_load("4-meifly/houses-brushing3.r")
ggplot(d) +
  scale_x_continuous("Variable", breaks = 1:7, labels = vars) +
  scale_y_continuous("Standardized Estimates") +
  annotate("rect", xmin = 5.9, xmax = 6.1, ymin = -0.45, ymax = -0.15, fill = NA, size = 2,
    colour = "black")
ggsave("4-meifly/houses-brushing3.pdf", width = 8, height = 5)

d <- dd_load("4-meifly/houses-brushing4.r")
ggplot(d) +
  scale_x_continuous("Degrees of freedom") +
  scale_y_continuous(expression(R ^ 2))
ggsave("4-meifly/houses-brushing4.pdf", width = 5, height = 5)

d <- dd_load("4-meifly/houses-brushing5.r")
ggplot(d) +
  scale_x_continuous("Residuals") +
  scale_y_continuous("Cooks D") +
  annotate("rect", xmin = -500000, xmax = -420000, ymin = 1.05, ymax = 1.15,
    fill = NA, size = 2, colour = "black")
ggsave("4-meifly/houses-brushing5.png", width = 4, height = 4)

d <- dd_load("4-meifly/houses-brushing6.r")
ggplot(d) +
  scale_x_continuous("Size") +
  scale_y_continuous("Price")
ggsave("4-meifly/houses-brushing6.png", width = 4, height = 4)

