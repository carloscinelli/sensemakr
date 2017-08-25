## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.width=6,
                      fig.height=6)

## ------------------------------------------------------------------------
library(sensemakr)
data("darfur")

## ------------------------------------------------------------------------
#devtools::install_github("chadhazlett/sensemakr", build_vignettes = TRUE)
# setwd("~/projects/sensemakr_fin/sensemakr/")
# devtools::build_vignettes() 

## ------------------------------------------------------------------------
lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
               pastvoted + hhsize_darfur + female + village, data = darfur)

## ------------------------------------------------------------------------
sense.out <- sensemakr(model=lm.out, treatment="directlyharmed")

## ------------------------------------------------------------------------
summary(sense.out)

## ------------------------------------------------------------------------
worstcaseinterpret(sense.out, scenarios=c(.1,.3), q=.5)

## ------------------------------------------------------------------------
plot(sense.out)

## ------------------------------------------------------------------------
plot(sense.out, showvars = list("pastvoted","female"))

## ------------------------------------------------------------------------
plot(sense.out, showvars = list("pastvoted","female"), contour="t-value")

## ------------------------------------------------------------------------
plot(sense.out, showvars = list("pastvoted","female"), contour="upper bound", lim=.35)
plot(sense.out, showvars = list("pastvoted","female"), contour="lower bound", lim=.35)

## ------------------------------------------------------------------------
plot(sense.out, type="worst-case")

## ------------------------------------------------------------------------
sense.out$treat.stats

## ------------------------------------------------------------------------
ls(sense.out$benchmarks)

## ------------------------------------------------------------------------
round(sense.out$benchmarks$benchmark_masked[,-1],4)

## ------------------------------------------------------------------------
sense.out$benchmarks$benchmark_group

## ------------------------------------------------------------------------
sense.grp.out= sensemakr(lm.out, treatment="directlyharmed", group_list = list(c("farmer_dar","herder_dar")))

## ------------------------------------------------------------------------
sense.grp.out$benchmarks$benchmark_group
plot(sense.grp.out, showvars=list("farmer_dar,herder_dar", "female"))

