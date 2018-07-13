#' Contour plot.
#'
#' Produces a sensitivity analysis contour plot. The produced plot maps how
#' observed effects could attenuate in the presence of an unobserved confounder
#' that is correlated with treatment and outcome.
#'
#' @param x A `sensemakr`  object to produce a contour plot from.
#' @param ... Additional plotting graphical parameters
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the bound plot
#' @param nlevels How many contour lines to produce
#' @param plot_t Logical, default FALSE describing whether to plot the
#' t-statistic (TRUE) or effect estimate (FALSE).
#' @param col.contour A color parameter for contour plot lines
#' @param col.line A color parameter for the threshold / critical value line
#' @param multipliers_y A vector of multipliers, k, describing how many times
#' as strong the hypothetical confounders are than the benchmark confounder
#' with respect to the outcome. Only usable with `sensemakr` objects that
#' contain benchmark variables.
#' @param multipliers_d A vector of multipliers, k, describing how many times
#' as strong the hypothetical confounders are than the benchmark confounder
#' with respect to the treatment. Only usable with `sensemakr` objects that
#' contain benchmark variables.
#'
#' @export
contour_plot.sensemakr = function(x, ...,
                                  lim = c(0, 0.4, 0.001),
                                  nlevels = 20,
                                  plot_t = FALSE,
                                  col.contour = "grey40",
                                  col.line = "red",
                                  multipliers_y = c(1, 2, 3),
                                  multipliers_d = multipliers_y
) {

  # Whether or not to thread through a benchmark
  if(!is.null(x$benchmark)) {
    benchmark_covariate = x$benchmark[, "variable"]
    benchmark_r2d = x$benchmark[, "r2d"]
    benchmark_r2y = x$benchmark[, "r2y"]
  } else {
    benchmark_covariate = NULL
    benchmark_r2d = NULL
    benchmark_r2y = NULL
  }

  # Call plot.
  # TODO: Remove requirement for models directly here by
  # passing in values we care about.
  contour_plot(
    estimate = x$treatment_effect[1],
    se = x$treatment_effect[2],
    dof = x$dof,
    r2d = benchmark_r2d,
    r2y = benchmark_r2y,
    benchmark_covariate = benchmark_covariate,
    lim = lim,
    nlevels = nlevels,
    plot_t = plot_t,
    col.contour = col.contour,
    col.line = col.line,
    multipliers_y = multipliers_y,
    multipliers_d = multipliers_d,
    ...
  )
}
