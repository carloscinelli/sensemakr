# Worked out example ------------------------------------------------------

# cleans workspace
rm(list = ls())

# library
library(sensemakr)

# loads data
data("darfur")

# fits model
model  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = darfur)

# treatment
D <- "directlyharmed"

# benchmark variables
X = c("herder_dar", "female", "age", "pastvoted", "farmer_dar")

# runs benchmarking etc
sense <- sensemakr(model, D, X)

# plots

## contour plot
plot(sense)
plot(sense, contour = "t-value")
plot(sense, contour = "lower bound")

## worst-case plot
plot(sense, type = "worst-case")
