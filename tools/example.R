# Worked out example ------------------------------------------------------

# devtools::install_github("chadhazlett/sensemakr",build_vignettes = TRUE,force=TRUE)

# cleans workspace
rm(list = ls())

# library
library(sensemakr)

# loads data
data("darfur")

# fits model
model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = darfur)

# runs benchmarking etc
sense = sensemakr(model=model, treatment="directlyharmed")

plot(sense,type='contour')
plot(sense,type='worst-case')

plot(sense,showvars='all',type='contour')
plot(sense,showvars='all',type='worst-case')

# standalone / exported
contourplot(sense)
contourplot(sense,showvars='all')
worstcaseplot(sense)
worstcaseplot(sense,showvars='all')



## contour plot
plot1_data = plot(sense)
plot(sense, lim=.2)
plot2_data = plot(sense, contour = "t-value")
plot3_data = plot(sense, contour = "lower-limit")
plot4_data = plot(sense, contour = "upper-limit")

## worst-case plot
plot5_data = plot(sense, type = "worst-case")
# note, let's me the benchmark ticks at the bottom much more visible
# and let's reduce number of lines that show and/or label them better.

# testing verbal outputs
interpret(sense)
interpret(sense, q = 0.5) #throws warning
interpret(sense, q = 0.6) #throws warning.

summary(sense) #needs more output/ to be different from interpret().



