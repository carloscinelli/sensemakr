#' Extreme scenario plots
#'
#' `extreme_plot` is a generic function to produce extreme scenario plots. The
#' function invokes particular methods which depend on the class of the first
#' argument. For documentation on using  `extreme_plot` with `sensemakr`
#' objects, check \link{extreme_plot.sensemakr}. For documentation on using
#' `extreme_plot` with direct numeric arguments, check
#' \link{extreme_plot.default}
#'
#' @param ... Arguments which will pass through to the methods invoked.
#' @export
extreme_plot = function(...) {
  UseMethod("extreme_plot")
}
