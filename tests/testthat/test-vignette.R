#######################################
# does not build vignette
#######################################
# devtools::install_github("chadhazlett/sensemakr")

library(sensemakr)


vignette(all=TRUE)
vignette(package = "sensemakr")

#######################################
# this does build vignette
#######################################
# devtools::install_github("chadhazlett/sensemakr",build_vignettes = TRUE,force=TRUE)

vignette(all=TRUE)
vignette('sensemakr',package = "sensemakr")


