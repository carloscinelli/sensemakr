#' Extreme scenario plot
#'
#' Produces an extreme scenario plot: a plot where unobserved confounders
#' explain all the left-out residual variance of the outcome. The x-axis
#' represents increasing partial R^2 of the unobserved covariate with the
#' treatment. Users can use this plot to make claims that unobserved
#' confounders that explain the entire residual variance are unlikely, so
#' persistence of effect magnitudes or signs even in the presence of them
#' suggests an overall robust effect.
#'
#' This mode of the function uses  `sensemakr` objects to produce the plot.
#' Users who wish to directly supply effect estimates manually should view
#' documentation on \link{extreme_plot.default}
#'
#' @param x A `sensemakr` object containing a benchmark covariate
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the bound plot
#' @param scenarios A vector of proportions of the residual variance explained
#' by the unobserved confounder. Defaults to `c(1, 0.8, 0.5)`
#' @param cex.legend A scaling factor for the legend text.
#' @param ... Additional graphical parameters
#'
#' @examples
#' # Creating a sensemakr object using the built-in `darfur` data
#' data(darfur)
#' sense.out = sensemakr(formula = peacefactor ~ directlyharmed + female +
#'                         village + age,
#'                       data = darfur,
#'                       treatment = "directlyharmed",
#'                       benchmark = "female")
#'
#' # Basic extreme scenario plot
#' extreme_plot(sense.out)
#'
#' @export
extreme_plot.sensemakr = function(x,
                                  lim = NULL,
                                  scenarios = c(1, 0.8, 0.5),
                                  cex.legend = 0.5,
                                  ...) {
  calls = list(...)

  if(is.null(x$benchmark) && !"r2d" %in% names(calls)) {
    stop("`sensemakr` objects used to create extreme value plots must have a ",
         "benchmark covariate specified.")
  }

  if("r2d" %in% names(calls)) {
    if(!is.null(x$benchmark)) {
      message("User specified `r2d` parameter, overriding `sensemakr` ",
              "benchmarks and using specified `r2d` parameter")
    }

    extreme_plot.default(
      estimate = x$treatment_effect[1],
      se = x$treatment_effect[2],
      dof = x$dof,
      lim = lim,
      scenarios = scenarios,
      cex.legend = cex.legend,
      ...
    )
  } else {
    extreme_plot.default(
      estimate = x$treatment_effect[1],
      se = x$treatment_effect[2],
      dof = x$dof,
      r2d = x$benchmark[, "r2d"],
      lim = lim,
      scenarios = scenarios,
      cex.legend = cex.legend,
      ...
    )
  }
}
