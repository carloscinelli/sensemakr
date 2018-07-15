#' Contour plot for `sensemakr` objects
#'
#' Produces a sensitivity analysis contour plot. The produced plot maps how
#' observed effects could attenuate in the presence of an unobserved confounder
#' that is correlated with treatment and outcome.
#'
#' By default, this plot will compare the unadjusted effect size estimate to
#' attenuated versions of the estimate under the presence of unobserved
#' confounding, using the benchmark variables specified in the `sensemakr`
#' object. The default comparison plots the effect under the presence of an
#' unobserved confounder at 1x, 2x, and 3x the "strength" of the benchmark
#' confounder. Users can override the benchmark confounders by specifying a
#' `benchmark_covariate` argument (a vector of character strings naming the
#' benchmarks to plot), and the multipliers by specifying `multipliers_y` and,
#' optionally, `mulitpliers_d`.
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
#' # Basic contour plot
#' contour_plot(sense.out)
#'
#' # Contour plot of t-values and critical threshold
#' contour_plot(sense.out, plot_t = TRUE)
#'
#' # Overriding multipliers
#' contour_plot(sense.out, multipliers_y = c(1, 1.5))
#'
#' # Overriding colors
#' contour_plot(sense.out, col.contour = "black", col.line = "blue")
#'
#' # Adding graphical parameters
#' contour_plot(sense.out, main = "Custom Title Plot")
#'
#' # Plotting only some of the benchmarks
#' sense.out = add_benchmark(sense.out, "age")
#' contour_plot(sense.out, benchmark_covariate = "female")
#'
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
  args = list(...)

  # Whether or not to thread through a benchmark
  if(!is.null(x$benchmark)) {
    # Either we grab all the benchmarks, or if user has asked for
    # some in specific, we grab those. We don't want the same benchmark
    # twice, so we do a 1:1 match
    indices = ifelse(!"benchmark_covariate" %in% names(args),
                     seq.int(nrow(x$benchmark)),
                     which(x$benchmark[, "variable"] %in%
                             args[["benchmark_covariate"]]))

    benchmark_covariate = x$benchmark[indices, "variable"]
    benchmark_r2d = x$benchmark[indices, "r2d"]
    benchmark_r2y = x$benchmark[indices, "r2y"]

    # Zap the old one so the ellipsis argument below doesn't pass this
    args[["benchmark_covariate"]] = NULL
  } else {
    benchmark_covariate = NULL
    benchmark_r2d = NULL
    benchmark_r2y = NULL
  }

  # Why the unusual call style? We modify the ellipsis arg, `...`. If the
  # resulting call uses ..., it won't use the modified version, so
  # benchmark_covariate will exist both as a named argument and as part of the
  # ellipsis. If we pass args as the final argument, it won't work as an
  # ellipsis. So, we're going to use do.call to pass the list of arguments in
  # order and named the way we expect. We do this by creating a merged list of
  # the arguments we would pass and the revised ellipsis from the parent call.
  new_args = c(
    list(
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
      multipliers_d = multipliers_d
    ),
    args
  )

  # Call plot.
  do.call(contour_plot, new_args)
}
