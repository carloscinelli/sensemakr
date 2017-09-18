# Worked out example ------------------------------------------------------

# devtools::install_github("chadhazlett/sensemakr",build_vignettes = TRUE,force=TRUE)

# cleans workspace
rm(list = ls())

# library
library(sensemakr)

# loads data
data("darfur")
?darfur

# fits model
model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = darfur)

# runs benchmarking etc
?sensemakr
sense = sensemakr(model=model, treatment="directlyharmed")

?print.sensemakr
sense
print(sense)

?plot.sensemakr


plot(sense,type='contour',ptlab=FALSE)
plot(sense,type='contour',ptlab=FALSE,lim=0.02)

plot(sense,type='contour')
plot(sense,type='worst-case',lim=0.6)
plot(sense,showvars='all',type='contour')
plot(sense,showvars='all',type='worst-case')

# standalone
?contourplot
?worstcaseplot

contourplot(sense)
contourplot(sense,showvars='all')
worstcaseplot(sense)
worstcaseplot(sense,showvars='all')



## other contour plots

plot(sense, contour = "t-value")
plot(sense, contour = "lower-limit")
out_plot = plot(sense, contour = "upper-limit")
ls(out_plot)


?summary.sensemakr

# if q and scenarios param specified in summary.sensemakr(...),
# carried forward to print.summary.sensemakr
# since it is print.summary.sensemakr(q,scenarios) that has these args in scope

summary(sense)
summary(sense,q=0.7)
summary(sense,q = 0.2, scenarios = 0.5)


?print.summary.sensemakr
print(summary(sense))
print(summary(sense),q=0.7)


?interpret
interpret(sense)
interpret(sense, q = 0.5)
interpret(sense, q = 0.6)

?worstcaseinterpret
worstcaseinterpret(sense,q = 0.2, scenarios = 0.5)


# docs
?sensemakr
?sensemakr.lm
?print.sensemakr
?summary.sensemakr
?print.summary.sensemakr
?plot.sensemakr
?contourplot
?worstcaseplot

?get_bias
?get_se
?get_t
?adjust_estimate
?t_to_r2
?group_r2
