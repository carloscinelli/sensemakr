## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.width=6,
                      fig.height=6)

## ------------------------------------------------------------------------
library(sensemakr)
data("darfur")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("chadhazlett/sensemakr", build_vignettes = TRUE)

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
plot(sense.out, showvars = list("pastvoted","female"), contour="upper-limit", lim=.35)
plot(sense.out, showvars = list("pastvoted","female"), contour="lower-limit", lim=.35)

## ------------------------------------------------------------------------
plot(sense.out, type="worst-case",lim=0.5)

## ------------------------------------------------------------------------
sense.out$treat.stats

## ------------------------------------------------------------------------
ls(sense.out$benchmarks)

## ------------------------------------------------------------------------
round(sense.out$benchmarks$benchmark_masked,4)

## ------------------------------------------------------------------------
sense.out$benchmarks$benchmark_group

## ------------------------------------------------------------------------
sense.grp.out = sensemakr(lm.out, treatment="directlyharmed",
                         group_list = list(c("farmer_dar","herder_dar")))

## ------------------------------------------------------------------------
sense.grp.out$benchmarks$benchmark_group
plot(sense.grp.out, showvars=list("farmer_dar,herder_dar", "female"))

## ------------------------------------------------------------------------
# turn off default labels
plot_out = contourplot(sense.out,ptlab=FALSE,lim=0.02)
head(plot_out$labels)  # contains original labels and positions

# overlay new algorithimically spaced labels
library(maptools)
# ?maptools::pointLabel
# "SANN" simulated annealing

with(plot_out$labels,
     maptools::pointLabel(x, y,
                labels = labels,
                method='SANN',
                offset = 1, cex = .8))

# compare to default labels of plot(sensemakr)
# contourplot(sense.out,ptlab=TRUE,lim=0.02)

## other "GA" genetic algorithm option
# with(plot_out$labels,
#      maptools::pointLabel(x, y,
#                 labels = labels,
#                 method='GA',
#                 offset = 1, cex = .8))


## ----eval=FALSE----------------------------------------------------------
#  # locator() method eg handpick placements
#  
#  # First, use default ptlab=TRUE to show default labels
#  # human book-keep, figure out order which points are which,
#  plot(sense,type='contour',ptlab=TRUE,lim=0.02)
#  
#  # one strategy: top to bottom along y-axis
#  # age pastvoted framerdar hhsize_darfur herder_dar
#  
#  # end-user handpicks x-y locations via ?locator()
#  # click plot device sequentially according to user's strategy
#  pts_pick = locator()
#  
#  str((plot_out$labels)$labels)
#  
#  # get index position of '(plot_out$labels)$labels'
#  # that matches ranking criteria: y-axis top to bottom
#  
#  ind_name_order = c(2,3,6,5,4)
#  lab_order = ((plot_out$labels)$labels)[ind_name_order]
#  lab_pos_manual = cbind(data.frame(pts_pick),lab_order)
#  
#  # 1) now plot() but toggle off ptlab
#  # 2) add text() using end-user created df 'lab_pos_manual'
#  plot(sense,type='contour',ptlab=FALSE,lim=0.02)
#  with(lab_pos_manual,text(x,y,lab_order,cex=0.6))
#  

