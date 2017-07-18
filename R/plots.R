
# graphics ----------------------------------------------------------------

##' @title Sensitivity plots
##' @description Several sensitivity plots.
##'
##' @param x sensemade object
##' @param type type of the plot
##' @param ... extra arguments
##'
##'
##' @return a ggplot object with the plot.
plot.sensemade <- function(x, type = "xxx", ...){
  switch(type,
         contour = contourplot(x, ...),
         worstcase = worstcaseplot(x, ...)
  )
}

contourplot <- function(...){
  #

}


worstcaseplot <- function(...){
  #

}
