################################################################################
# Leave custom label placements to end-user as extra step
# AFTER plot(sensemakr)
# eg 'examples not run' or a 'vignette' section
################################################################################

# NOTE: if the fundamental issue is the points themseleves are overlapping
# even harder to prevent the labels of points from overlapping


# ez options to adjust for label overlap
# 1) plot(lim=.) to zoom in
# 2) plot(ptlab=FALSE) and locator() to choose label placement themselves


plot(sense,type='contour',ptlab=FALSE)
plot(sense,type='contour',ptlab=FALSE,lim=0.02)
plot_out = contourplot(sense,ptlab=FALSE,lim=0.02)


################################################
# locator() method
# eg handpick placements,
# ok if set is small
# gives a lot of spatial control
################################################

# First, use default ptlab=TRUE to show default labels
# to figure out order
# human book-keep, which points are which
plot(sense,type='contour',ptlab=TRUE,lim=0.02)

## top to bottom along y-axis
# age
# pastvoted
# framerdar
# hhsize_darfur
# herder_dar

# end-user handpicks x-y locations via ?locator()
pts_pick = locator()

((plot_out$labels)$labels)

# get index position of '(plot_out$labels)$labels'
# that matches ranking criteria: y-axis top to bottom

ind_name_order = c(2,3,6,5,4)
lab_order = ((plot_out$labels)$labels)[ind_name_order]
lab_pos_manual = cbind(data.frame(pts_pick),lab_order)

# 1) now plot() but toggle off ptlab
# 2) add text() using df 'lab_pos_manual' end-user created
plot(sense,type='contour',ptlab=FALSE,lim=0.02)
with(lab_pos_manual,text(x,y,lab_order,cex=0.6))

################################################
# post-processing label coordinates
# using external r packages
# 1) ?maptools::pointLabel
# 2) ?FField::FFieldPtRep
################################################

#######################
# algorithmic method depending on 'maptools' package
# ?maptools::pointLabel
#######################

library(maptools)
# needs external dependencies like 'rgeos',
# that is why leave to end-user
?maptools::pointLabel

# default labels of plot(sensemakr)
plot_out = contourplot(sense,ptlab=TRUE,lim=0.02)

(plot_out$labels)

plot_out = contourplot(sense,ptlab=FALSE,lim=0.02)

# "SANN" simulated annealing
with(plot_out$labels,
     pointLabel(x, y,
                labels = labels,
                method='SANN',
                offset = 1, cex = .8)
     )

# "GA" genetic algorithm
with(plot_out$labels,
     pointLabel(x, y,
                labels = labels,
                method='GA',
                offset = 1, cex = .8)
     )



#######################
# FField method
# install.packages("FField")
# ?FField::FFieldPtRep
# Force field simulation of interaction of set of points.
#######################

plot_out = contourplot(sense,ptlab=FALSE,lim=0.02)

?FField::FFieldPtRep

labels = (plot_out$labels)$labels
x_orig = (plot_out$labels)$x
y_orig = (plot_out$labels)$y

(plot_out$labels)[,c('x','y')]

x.fact = 100 / max(x_orig)
y.fact = 100 / max(y_orig)

coords = cbind(x_orig * x.fact,y_orig * y.fact)
coord_ff = FField::FFieldPtRep(coords,attr.fact = 0.2)

x.t <- coord_ff$x / x.fact
y.t <- coord_ff$y / y.fact


contourplot(sense,ptlab=FALSE,lim=0.02)
text(cbind(x.t,y.t),labels,cex=0.6)
# up to user to adjust arguments in
# ?FField::FFieldPtRep

contourplot(sense,ptlab=TRUE,lim=0.02)



#######################
# wordcloud::wordlayout() method
# algorithmic spiral of labels
# dependency not available
# install.packages('wordcloud',dependencies = TRUE)
# ERROR: dependency ‘slam’ is not available for package ‘tm’
#######################
