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

# benchmark variables
X = c("herder_dar", "female", "age", "pastvoted", "farmer_dar")

# runs benchmarking etc
sense <- sensemakr(model=model, D="directlyharmed", benchmarks=X)

# plots

## contour plot
plot1_data <- plot(sense)
plot2_data <- plot(sense, contour = "t-value")
plot3_data <- plot(sense, contour = "lower bound")
plot4_data <- plot(sense, contour = "upper bound")

## worst-case plot
plot5_data <- plot(sense, type = "worst-case")
# note, let's me the benchmark ticks at the bottom much more visible
# and let's reduce number of lines that show and/or label them better.

# testing verbal outputs
interpret(sense)
interpret(sense, q = 0.5) #throws warning
interpret(sense, q = 0.6) #throws warning.

summary(sense) #needs more output/ to be different from interpret().



