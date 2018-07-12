#' Contour plot
#'
#' `contour_plot` is a generic function to produce sensitivity analysis
#' contour plots. The function invokes particular methods which depend on
#' the class of the first argument. For documentation on using  `contour_plot`
#' with `sensemakr` objects, check \link{contour_plot.sensemakr}. For
#' documentation on using `contour_plot` with direct numeric arguments, check
#' \link{contour_plot.default}
#'
#' @param ... Arguments which will pass through to the methods invoked.
#' @export
contour_plot = function(...) {
  UseMethod("contour_plot")
}
