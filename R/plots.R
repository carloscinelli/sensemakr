
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

contourplot <- function(x, type = "estimate", nlevels = 15, pch = 20, cex = 0.5, lim = NULL,
                         xlab = "Hypothetical partial R2 of unobserved confounder with the treatment",
                         ylab = "Hypothetical partial R2 of unobserved confounder withthe outcome",
                         main = "Sensitivity of estimate to unobserved confounder\nContours of adjusted estimates",
                         ...){
  r2y <- x$benchmarks$r2y
  r2d <- x$benchmarks$r2d
  estimate <- x$sensitivity$estimate
  k <- x$sensitivity$se * sqrt(x$sensitivity$dof)

  if (is.null(lim)) lim <- max(c(r2y, r2d), na.rm = TRUE) + 0.1
  s <- seq(0, lim, by = 0.001)
  z <- estimate - outer(s, s, biasR2, k = k)

  contour(s, s, z, nlevels = nlevels,
          xlab = xlab,
          ylab = ylab,
          main = main, ...)
  points(r2d, r2y, pch = pch, cex = cex)
}


worstcaseplot <- function(...){
  #

}
