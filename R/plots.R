
# Plots -------------------------------------------------------------------


# generic plot function ---------------------------------------------------

#' Plots for sensitivity analysis to unobserved confounders
#'
#' This is a generic method which dispatches to various plotting functions for
#' `sensemakr` objects. By default, the plotting function chosen is
#' \link{contour_plot.sensemakr}. Using the `type` argument, users can also
#' select \link{extreme_plot.sensemakr} and \link{ovb_plot.sensemakr} plots.
#'
#' @param x A sensemakr object
#' @param type A character string reading "contour", "extreme", or "ovb".
#' @param ... Further plotting arguments described in documentation for
#' \link{contour_plot.sensemakr}, \link{extreme_plot.sensemakr}, or
#' \link{ovb_plot.sensemakr}.
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
#' # Contour plot
#' plot(sense.out)
#' plot(sense.out, type="contour")
#'
#' # Extreme Scenario plot
#' plot(sense.out, type="extreme")
#'
#' # Traditional, unscaled OVB plot
#' plot(sense.out, type="ovb")
#'
#' @export
plot.sensemakr = function(x, type = "contour", ...) {
  if (is.null(type) || !type %in% c("contour", "extreme", "ovb")) {
    stop("`type` argument to `plot.sensemakr` must be 'contour', 'extreme', ",
         "or 'ovb'.")
  }

  # Call the dispath function of interest
  switch(type,
         "contour" = dispatch_contour,
         "extreme" = dispatch_extreme,
         "ovb" = dispatch_ovb)(x, ...)
}

dispatch_contour = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  ovb_contour_plot(x, ...)
}

dispatch_extreme = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  ovb_extreme_plot(x, ...)
}




# contour plot ------------------------------------------------------------


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
#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot = function(...) {
  UseMethod("ovb_contour_plot")
}




#' Contour plot
#'
#' Produces a sensitivity analysis contour plot. `contour_plot` can be called by
#' providing either a series of numerical parameters (`estimate`, `se`, `dof`,
#' and optionally `r2dz.x` and `r2yz.dx`) or by providing references to models
#' (`effect_model`, `treatment_covariate`, and optionally `treatment_model`
#' and `benchmark_covariate`). The produced plot maps how observed effects
#' could attenuate in the presence of an unobserved confounder that is
#' correlated with treatment and outcome. See also \link{contour_plot.sensemakr}
#' to easily produce a contour plot from a `sensemakr` object.
#'
#' By default, the `r2dz.x` and `r2yz.dx` (or the same quantities derived from
#' `benchmark_covariate`) will be used to construct hypothetical unobserved
#' confounders. The original effect size will be attenuated by the unobserved
#' confounder: the magnitude of the attenuation will depend on the degree of
#' confounding. The parameters `multipliers_y` and `multipliers_d` specify how
#' many times as "strong" the unobserved confounder should be compared to the
#' reference point. The default is c(1, 2, 3), implying that unobserved
#' confounders 1, 2, and 3 times as strong as the reference confounder will be
#' plotted.
#'
#' @examples
#' # Simply plotting a contour plot with no confounders
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200)
#'
#' # Adding a confounder that has a partial R^2 of 0.1 with both treatment and
#' # outcome
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200, r2dz.x = 0.1, r2yz.dx = 0.1)
#'
#' # Plotting the t-statistic instead of the effect point estimate
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200,
#'              r2dz.x = 0.1, r2yz.dx = 0.1, plot_t = TRUE)
#'
#' # Changing graphical parameters
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200,
#'              r2dz.x = 0.1, r2yz.dx = 0.1, col.line = "blue",
#'              col.contour = "black", nlevels = 10,
#'              main = "Contour Plot Example")
#'
#' # Producing a contour plot using `lm` objects: note that users may find it
#' # easier to create a `sensemakr` object and use the `contour_plot` method on
#' # that object
#' data(darfur)
#' model.outcome = lm(peacefactor ~ directlyharmed + female + age + village,
#'                    data = darfur)
#' model.treat = lm(directlyharmed ~ female + age + village,
#'                  data = darfur)
#' contour_plot(effect_model = model.outcome,
#'              treatment_covariate = "directlyharmed",
#'              treatment_model = model.treat,
#'              benchmark_covariate = "female")
#'
#' @param estimate An estimated effect magnitude
#' @param se The standard error of the estimated effect magnitude
#' @param dof The residual degrees of freedom of the regression producing the
#' effect estimate
#' @param r2dz.x Partial R^2 with respect to the treatment
#' @param r2yz.dx Partial R^2 with respect to the outcome variable
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the bound plot
#' @param nlevels How many contour lines to produce
#' @param plot_t Logical, default FALSE describing whether to plot the
#' t-statistic (TRUE) or effect estimate (FALSE).
#' @param col.contour A color parameter for contour plot lines
#' @param col.line A color parameter for the threshold / critical value line
#' @param multipliers_y A vector of multipliers, k, describing how many times
#' as strong the hypothetical confounders are than the benchmark confounder
#' with respect to the outcome
#' @param multipliers_d A vector of multipliers, k, describing how many times
#' as strong the hypothetical confounders are than the benchmark confounder
#' with respect to the treatment
#' @param ... Additional plotting graphical parameters
#' @param effect_model An lm object describing the relationship Y ~ D + X.
#' If provided, `estimate`, `se`, and `dof` will be extracted from this.
#' @param treatment_model An lm object describing the relationship D ~ X.
#' If provided, the benchmark confounder will be extracted from this.
#' @param treatment_covariate The quoted character name of a variable in the `lm`
#' object `effect_model`, describing the treatment variable
#' @param benchmark_covariate The quoted character name of a variable in the
#' `lm` objects, describing the benchmark confounder
#'
#' @importFrom graphics contour points text
#' @importFrom stats coef
#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.numeric = function(estimate,
                                    se,
                                    dof,
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    r2dz.x = NULL,
                                    r2yz.dx = NULL,
                                    bound_label = NULL,
                                    plot_t = FALSE,
                                    t.threshold = 2,
                                    lim = c(0, 0.4, 0.001),
                                    nlevels = 20,
                                    col.contour = "grey40",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    label.bump = 0.02,
                                    ...) {

  error_estimate(estimate)
  error_limit(lim)

  # Set up the grid for the contour plot
  grid_values = seq(lim[1], lim[2], by = lim[3])

  # Are we plotting t or bias in r2?
  if (!plot_t) {
    z_axis = t(outer(grid_values, grid_values,
                     FUN = "adjusted_estimate",
                     estimate = estimate,
                     se = se,
                     dof = dof,
                     reduce = reduce))
  } else {
    z_axis = t(outer(grid_values, grid_values,
                     FUN = "adjusted_t",
                     se = se, dof = dof, estimate = estimate))
  }

  # Aesthetic: Override the 0 line; basically, check which contour curve is
  # the zero curve, and override that contour curve with alternate aesthetic
  # characteristics
  default_levels = pretty(range(z_axis), nlevels)
  threshold = ifelse(plot_t, t.threshold, estimate.threshold)
  line_color = ifelse(default_levels == threshold,
                      col.thr.line,
                      col.contour)
  line_type = ifelse(default_levels == threshold,
                     2, 1)
  line_width = ifelse(default_levels == threshold,
                      2, 1)

  # Plot contour plot:
  contour(
    grid_values, grid_values, z_axis, nlevels = nlevels,
    xlab = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                            " confounder(s) with the treatment")),
    ylab = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                            " confounder(s) with the outcome")),
    cex.main = 1, cex.lab = 1, cex.axis = 1,
    col = line_color, lty = line_type, lwd = line_width,
    ...)

  # Add the point of the initial estimate.
  points(0, 0, pch = 17, col = "black", cex = 1)
  plot_estimate = ifelse(plot_t, estimate / se, estimate)
  text(0.0, 0.02,
       paste0("Unadjusted\n(",
              signif(plot_estimate, 2),
              ")"),
       cex = 1)

  # add bounds
  if (!is.null(r2dz.x)) {

    error_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)

    add_bound_to_contour(estimate = estimate,
                         se = se,
                         dof = dof,
                         treatment = treatment,
                         benchmark_covariate = covariate,
                         r2dz.x = r2dz.x,
                         r2yz.dx = r2yz.dx,
                         bound_label = bound_label,
                         plot_t = plot_t,
                         label.text = label.text,
                         label.bump = label.bump)
  }
}


#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.lm = function(model,
                               treatment,
                               benchmark_covariates = NULL,
                               kd = 1,
                               ky = kd,
                               reduce = TRUE,
                               estimate.threshold = 0,
                               r2dz.x = NULL,
                               r2yz.dx = NULL,
                               bound_label = NULL,
                               plot_t = FALSE,
                               t.threshold = 2,
                               lim = c(0, 0.4, 0.001),
                               nlevels = 20,
                               col.contour = "grey40",
                               col.thr.line = "red",
                               label.text = TRUE,
                               label.bump = 0.02,
                               ...) {


  error_multipliers(ky = ky, kd = kd)

  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  model_data <- model_helper(model, covariate = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof


  bounds <-  data.frame(r2dz.x = r2dz.x,
                        r2yz.dx = r2yz.dx,
                        bound_label = bound_label)

  if (!is.null(benchmark_covariates)) {

    # we will need to add an option for the bound type
    bench_bounds <- ovb_partial_r2_bound(model = model,
                                         treatment = treatment,
                                         benchmark_covariates = benchmark_covariates,
                                         kd = kd,
                                         ky = ky)
    bounds <- rbind(bounds, bench_bounds)
  }

  ovb_contour_plot(estimate = estimate,
               se = se,
               dof = dof,
               reduce = reduce,
               estimate.threshold = estimate.threshold,
               r2dz.x = bounds$r2dz.x,
               r2yz.dx = bounds$r2yz.dx,
               bound_label = bounds$bound_label,
               plot_t = plot_t,
               t.threshold = t.threshold,
               lim = lim,
               nlevels = nlevels,
               col.contour = col.contour,
               col.thr.line = col.thr.line,
               label.text = label.text,
               label.bump = label.bump,
               ...)


}

#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.formula = function(formula,
                                    data,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    ky = kd,
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    plot_t = FALSE,
                                    t.threshold = 2,
                                    lim = c(0, 0.4, 0.001),
                                    nlevels = 20,
                                    col.contour = "grey40",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    ...) {
  if (is.null(treatment) || !treatment %in% all.vars(formula) ||
      (!is.null(data) && !treatment %in% colnames(data)) ||
      (is.null(data) && !exists(as.character(treatment)))
  ) {
    stop("You must provide a `treatment` variable present in the model ",
         "`formula` and `data` data frame.")
  }
  if (!is.null(data) && (!is.data.frame(data) || nrow(data) < 1)) {
    stop("The provided `data` argument must be a data frame with at least ",
         "one row.")
  }

  error_multipliers(ky = ky, kd = kd)

  lm.call <- call("lm", formula = substitute(formula), data = substitute(data))
  outcome_model = eval(lm.call)

  ovb_contour_plot(model = outcome_model,
                   treatment = treatment,
                   benchmark_covariates = benchmark_covariates,
                   kd = kd,
                   ky = ky,
                   reduce = reduce,
                   estimate.threshold = estimate.threshold,
                   plot_t = plot_t,
                   t.threshold = t.threshold,
                   lim = lim,
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   ...)
}


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
#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.sensemakr = function(x, ...,
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
  if (!is.null(x$benchmark)) {
    # Either we grab all the benchmarks, or if user has asked for
    # some in specific, we grab those. We don't want the same benchmark
    # twice, so we do a 1:1 match
    indices = ifelse(!"benchmark_covariate" %in% names(args),
                     seq.int(nrow(x$benchmark)),
                     which(x$benchmark[, "variable"] %in%
                             args[["benchmark_covariate"]]))

    benchmark_covariate = x$benchmark[indices, "variable"]
    benchmark_r2dz.x = x$benchmark[indices, "r2dz.x"]
    benchmark_r2yz.dx = x$benchmark[indices, "r2yz.dx"]

    # Zap the old one so the ellipsis argument below doesn't pass this
    args[["benchmark_covariate"]] = NULL
  } else {
    benchmark_covariate = NULL
    benchmark_r2dz.x = NULL
    benchmark_r2yz.dx = NULL
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
      r2dz.x = benchmark_r2dz.x,
      r2yz.dx = benchmark_r2yz.dx,
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


add_bound_to_contour <- function(estimate,
                                 se,
                                 dof,
                                 treatment,
                                 benchmark_covariate,
                                 r2dz.x, r2yz.dx,
                                 bound_label = NULL,
                                 plot_t = FALSE,
                                 label.text = TRUE,
                                 label.bump = 0.02,
                                 ...){

  for (i in seq.int(length(r2dz.x))) {


    # Add the point on the contour:
    points(r2dz.x[i], r2yz.dx[i],
           pch = 23, col = "black", bg = "red",
           cex = 1, font = 1)

    # Get the attenuation of the effect
    if (!plot_t) {
      adjusted_estimate = adjusted_estimate(estimate,
                                            r2yz.dx = r2yz.dx[i],
                                            r2dz.x = r2dz.x[i],
                                            se = se, dof = dof)
    } else {
      adjusted_estimate = adjusted_t(r2yz.dx = r2yz.dx[i],
                                     r2dz.x = r2dz.x[i],
                                     estimate = estimate,
                                     se = se, dof = dof)
    }

    label = paste0(bound_label[i], "\n(", signif(adjusted_estimate, 2), ")")

    # Add the label text
    if (label.text)
      text(r2dz.x[i], r2yz.dx[i] + label.bump,
           labels = label,
           cex = 1.1, font = 1)
  }

}


# extreme plot ------------------------------------------------------------

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
#' @rdname ovb_extreme_plot
#' @export
ovb_extreme_plot = function(...) {
  UseMethod("ovb_extreme_plot")
}


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
#' The easiest way to use `extreme_plot` is by creating a `sensemakr` object
#' and using the \link{extreme_plot.sensemakr} function on it. This mode of the
#' function is for users who want to directly pass estimates.
#'
#' @param estimate An estimated effect magnitude
#' @param se The standard error of the estimated effect magnitude
#' @param dof The residual degrees of freedom of the regression producing the
#' effect estimate
#' @param r2d Partial R^2 with respect to the treatment of a known, observed
#' confounder. This is used to produce a rug inscription on the x-axis in order
#' to benchmark the unobserved confounder versus known confounders.
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the bound plot
#' @param scenarios A vector of proportions of the residual variance explained
#' by the unobserved confounder. Defaults to `c(1, 0.8, 0.5)`
#' @param cex.legend A scaling factor for the legend text.
#' @param ... Additional graphical parameters
#'
#' @examples
#' # Basic extreme scenario plot
#' extreme_plot(estimate = 0.09589905, se = 0.02318221, dof = 783,
#'              r2d = 0.008106797)
#'
#' @importFrom graphics abline lines legend rug plot
#' @rdname ovb_extreme_plot
#' @export
ovb_extreme_plot.numeric = function(estimate,
                                    se,
                                    dof,
                                    r2dz.x = NULL,
                                    r2yz.dx = c(1, 0.75, 0.5),
                                    reduce = TRUE,
                                    threshold = 0,
                                    lim = min(c(r2dz.x + 0.1, 0.5)),
                                    cex.legend = 0.5, ...) {

  # if (is.null(lim)) {
  #   if (is.null(r2dz.x)) {
  #     stop("If `lim` is not provided, `r2dz.x` must be to automatically pick a ",
  #          "plot limit.")
  #   }
  #   lim = max(r2dz.x, na.rm = TRUE) + 0.1
  # }

  # x-axis values: R^2 of confounder(s) with treatment
  r2d_values = seq(0, lim, by = 0.001)

  # Iterate through scenarios:
  for (i in seq.int(length(r2yz.dx))) {
    y = adjusted_estimate(estimate, r2yz.dx = r2yz.dx[i],
                     r2dz.x = r2d_values,
                     se = se,
                     dof = dof, reduce = reduce)
    # Initial plot
    if (i == 1) {
      if (estimate < 0) {
        ylim = rev(range(y))
        } else {
          ylim = range(y)
        }
      plot(
        r2d_values, y, type = "l", bty = "L",
        ylim = ylim,
        xlab = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                                 " confounder(s) with the treatment")),
        ylab = "Adjusted effect estimate",
        ...)
      abline(h = threshold, col = "red", lty = 5)
    } else {
      # Add plot lines
      lines(r2d_values, y, lty = i + 1)
    }

  }

  legend(
    x = "topright",
    inset = 0.05,
    lty = c(seq_len(length(r2yz.dx)), 5),
    col = c(rep("black", length(r2yz.dx)),
            "red"),
    # bty = "n",
    legend = paste0(r2yz.dx * 100, "%"),
    ncol = length(r2yz.dx) + 1,
    title = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                             " confounder(s) with the outcome")),
    cex = cex.legend
  )

  if (!is.null(r2dz.x))
    rug(x = r2dz.x, col = "red", lwd = 2)
}

#' @rdname ovb_extreme_plot
#' @export
ovb_extreme_plot.lm <- function(model,
                                treatment,
                                benchmark_covariates = NULL,
                                kd = 1,
                                r2yz.dx = c(1, 0.75, 0.5),
                                r2dz.x = NULL,
                                reduce = TRUE,
                                threshold = 0,
                                lim = min(c(r2dz.x + 0.1, 0.5)),
                                cex.legend = 0.5, ...){
  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  model_data <- model_helper(model, covariate = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  ovb_extreme_plot(estimate = estimate,
                   se = se,
                   dof = dof,
                   r2dz.x = r2dz.x,
                   r2yz.dx = r2yz.dx,
                   reduce = reduce,
                   threshold = threshold,
                   lim = lim,
                   cex.legend = cex.legend,
                   ...)

  if (!is.null(benchmark_covariates)) {
    for (covariate in benchmark_covariates) {

      # TODO: We will need to make bound_type an option later
      bounds <- ovb_bounds(model = model,
                           treatment = treatment,
                           benchmark_covariates = covariate,
                           kd = kd,
                           ky = 1 ,
                           bound_type = "partial r2")

      rug(x = bounds$r2dz.x, col = "red", lwd = 2)
    }
  }

}


#' @rdname ovb_extreme_plot
#' @export
ovb_extreme_plot.formula = function(formula,
                                    data,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    r2yz.dx = c(1, 0.75, 0.5),
                                    r2dz.x = NULL,
                                    reduce = TRUE,
                                    threshold = 0,
                                    lim = min(c(r2dz.x + 0.1, 0.5)),
                                    cex.legend = 0.5, ...) {
  if (is.null(treatment) || !treatment %in% all.vars(formula) ||
      (!is.null(data) && !treatment %in% colnames(data)) ||
      (is.null(data) && !exists(as.character(treatment)))
  ) {
    stop("You must provide a `treatment` variable present in the model ",
         "`formula` and `data` data frame.")
  }
  if (!is.null(data) && (!is.data.frame(data) || nrow(data) < 1)) {
    stop("The provided `data` argument must be a data frame with at least ",
         "one row.")
  }

  lm.call <- call("lm", formula = substitute(formula), data = substitute(data))
  outcome_model = eval(lm.call)

  ovb_extreme_plot(model = outcome_model,
                   treatment = treatment,
                   benchmark_covariates = benchmark_covariates,
                   kd = kd,
                   r2yz.dx = r2yz.dx,
                   r2dz.x = r2dz.x,
                   reduce = reduce,
                   threshold = threshold,
                   lim = lim,
                   cex.legend = cex.legend, ...)
}

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
#' @rdname ovb_extreme_plot
#' @export
ovb_extreme_plot.sensemakr = function(x,
                                  lim = NULL,
                                  scenarios = c(1, 0.75, 0.5),
                                  cex.legend = 0.5,
                                  ...) {
  calls = list(...)

  if (is.null(x$benchmark) && !"r2d" %in% names(calls)) {
    stop("`sensemakr` objects used to create extreme value plots must have a ",
         "benchmark covariate specified.")
  }

  if ("r2d" %in% names(calls)) {
    if (!is.null(x$benchmark)) {
      message("User specified `r2d` parameter, overriding `sensemakr` ",
              "benchmarks and using specified `r2d` parameter")
    }

    ovb_extreme_plot(
      estimate = x$treatment_effect[1],
      se = x$treatment_effect[2],
      dof = x$dof,
      lim = lim,
      scenarios = scenarios,
      cex.legend = cex.legend,
      ...
    )
  } else {
    ovb_extreme_plot(
      estimate = x$treatment_effect[1],
      se = x$treatment_effect[2],
      dof = x$dof,
      r2dz.x = x$benchmark[, "r2dz.x"],
      lim = lim,
      scenarios = scenarios,
      cex.legend = cex.legend,
      ...
    )
  }
}


# sanity checkers ---------------------------------------------------------



error_estimate = function(estimate) {
  # Error if we don't have an estimate or if it's non-numeric
  if (is.null(estimate)) {
    stop("You must supply either a `model` and `treatment_covariate` to ",
         "extract an estimate or a directly supplied `estimate` argument")
  }
  if (!is.numeric(estimate)) {
    stop("The estimated effect must be numeric.")
  }
}

error_r2 = function(r2dz.x, r2yz.dx) {
  # Check r2dz.x / r2yz.dx
  if (is.null(r2dz.x) != is.null(r2yz.dx)) {
    stop("Either both `r2dz.x` and `r2yz.dx` must be provided or neither.")
  }

  if (!is.null(r2dz.x)) {
    if (any(!is.numeric(r2dz.x) | r2dz.x < 0 | r2dz.x > 1)) {
      stop("`r2dz.x` must be a number between zero and one, if provided")
    }

    if (length(r2dz.x) != length(r2yz.dx)) {
      stop("Lengths of `r2dz.x` and `r2yz.dx` must match.")
    }
  }

  if (!is.null(r2yz.dx) &&
     any(!is.numeric(r2yz.dx) | r2yz.dx < 0 | r2yz.dx > 1)) {
    stop("`r2yz.dx` must be a number between zero and one, if provided")
  }
}

error_limit = function(lim) {
  # Error if our grid limit is wrong.
  if (any(!is.numeric(lim) | lim < 0 | lim > 1) ||
     lim[2] <= lim[1] ||
     length(lim) != 3) {
    stop("Plot grid must consist of three numbers: lim = c(start, end, by)")
  }
}

error_multipliers = function(ky, kd) {
  # Error if multipliers are wrong.
  if ( any(!is.numeric(ky)) || any(!is.numeric(kd)) ||
     any(ky < 0) || any(kd < 0)) {
    stop("`ky` and `kd` must be vectors of non-negative ",
         "numbers")
  }
  if (length(ky) != length(kd)) {
    stop("`ky` and `kd` must be the same length.")
  }
}

