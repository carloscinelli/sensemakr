#' Traditional omitted variable bias plot
#'
#' This function produces a traditional omitted variable bias plot, which plots
#' the impact of an omitted variable on the relationship on interest contingent
#' on imbalance between treatment and control groups and a relationship between
#' the hypothetical variable and the outcome. We do not recommend using this
#' function, but provide it for compatibility with previous approaches. Please
#' note that this function is not exported and must be accessed through the
#' `sensemakr` package namespace. `ovb_plot` is a generic function to produce
#' omitted variable plots. The function invokes particular methods which depend
#' on the class of the first argument. For documentation on using `ovb_plot`
#' with `sensemakr` objects, check \link{ovb_plot.sensemakr}. For
#' documentation on using `ovb_plot` with direct numeric arguments, check
#' \link{ovb_plot.default}
#' @param ... Arguments which will pass through to the methods invoked.
#' @export
ovb_plot = function(...) {
  UseMethod("ovb_plot")
}
