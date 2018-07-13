#' Contour plot
#'
#' Produces a sensitivity analysis contour plot. `contour_plot` can be called by
#' providing either a series of numerical parameters (`estimate`, `se`, `dof`,
#' and optionally `r2d` and `r2y`) or by providing references to models
#' (`effect_model`, `treatment_covariate`, and optionally `treatment_model`
#' and `benchmark_covariate`). The produced plot maps how observed effects
#' could attenuate in the presence of an unobserved confounder that is
#' correlated with treatment and outcome. See also \link{contour_plot.sensemakr}
#' to easily produce a contour plot from a `sensemakr` object.
#'
#' By default, the `r2d` and `r2y` (or the same quantities derived from
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
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200, r2d = 0.1, r2y = 0.1)
#'
#' # Plotting the t-statistic instead of the effect point estimate
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200,
#'              r2d = 0.1, r2y = 0.1, plot_t = TRUE)
#'
#' # Changing graphical parameters
#' contour_plot(estimate = 1.8, se = 0.4, dof = 200,
#'              r2d = 0.1, r2y = 0.1, col.line = "blue",
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
#' @param r2d Partial R^2 with respect to the treatment
#' @param r2y Partial R^2 with respect to the outcome variable
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
#' @export
contour_plot.default = function(estimate = NULL,
                                se = NULL,
                                dof = NULL,
                                r2d = NULL,
                                r2y = NULL,
                                lim = c(0, 0.4, 0.001),
                                nlevels = 20,
                                plot_t = FALSE,
                                col.contour = "grey40",
                                col.line = "red",
                                multipliers_y = c(1, 2, 3),
                                multipliers_d = multipliers_y,
                                ...,
                                effect_model = NULL,
                                treatment_model = NULL,
                                treatment_covariate = NULL,
                                benchmark_covariate = NULL) {

  # In general we want the user to pass us an estimate. But if they would
  # rather pass us a model, let's use the model/covariate to fill the estimate.
  if(!is.null(effect_model) &&
     !is.null(treatment_covariate) && "lm" %in% class(effect_model)) {
    estimate = coef(summary(effect_model))[treatment_covariate, "Estimate"]
  }

  error_estimate(estimate)

  # In general we want a user to pass a partial R^2 D and R^2 Y for a
  # hypothetical confounder. But if they would rather pass two models and a
  # covariate name present in both, we can calculate these.
  if(!is.null(effect_model) && !is.null(treatment_model) &&
     !is.null(benchmark_covariate) && "lm" %in% class(effect_model) &&
     "lm" %in% class(treatment_model)) {
    r2_effect = r_squared_helper(effect_model)
    r2_treatment = r_squared_helper(treatment_model)

    if(!all(benchmark_covariate %in% names(r2_effect)) ||
       !all(benchmark_covariate %in% names(r2_treatment))) {
      missing_cov = benchmark_covariate[
        which(!benchmark_covariate %in% names(r2_effect) |
                !benchmark_covariate %in% names(r2_treatment))]

      stop("Could not locate the supplied `benchmark_covariate` ",
           paste(missing_cov, collapse=", "),
           "in provided models.")
    }

    r2d = r2_treatment[benchmark_covariate]
    r2y = r2_effect[benchmark_covariate]
  }

  error_r2(r2d, r2y)

  # In general we want user to pass SE / DOF, but if they would rather pass
  # a effect_model and treatment_covariate, we can calculate these.
  if(!is.null(effect_model) && !is.null(treatment_covariate) &&
     "lm" %in% class(effect_model)) {
    effect_result = model_helper(effect_model)
    dof = effect_result$degrees_of_freedom
    se = effect_result$se[treatment_covariate]
  }

  if(is.null(se) || is.null(dof)) {
    stop("You must either supply values for `se` and `dof` or supply an ",
         "`effect_model` model object and a `treatment_covariate` to ",
         "calculate them")
  }

  error_limit(lim)
  error_multipliers(multipliers_y, multipliers_d)

  # Set up the grid for the contour plot
  grid_values = seq(lim[1], lim[2], by = lim[3])
  # Are we plotting t or bias in r2?
  if(!plot_t) {
    z_axis = t(estimate - outer(grid_values, grid_values,
                                FUN = "bias_in_r2",
                                se = se, dof = dof))
  } else {
    z_axis = t(outer(grid_values, grid_values,
                     FUN = "t_r2",
                     se = se, dof = dof, estimate = estimate))
  }

  # Aesthetic: Override the 0 line; basically, check which contour curve is
  # the zero curve, and override that contour curve with alternate aesthetic
  # characteristics
  default_levels = pretty(range(z_axis), nlevels)
  threshold = ifelse(plot_t, 2, 0)
  line_color = ifelse(default_levels == threshold,
                      col.line,
                      col.contour)
  line_type = ifelse(default_levels == threshold,
                     2, 1)
  line_width = ifelse(default_levels == threshold,
                      2, 1)

  # Plot contour plot:
  contour(
    grid_values, grid_values, z_axis, nlevels = nlevels,
    xlab = "Hypothetical partial R^2 of confounder with treatment",
    ylab = "Hypothetical partial R^2 of confounder with outcome",
    cex.main = 1, cex.lab = 1, cex.axis = 1,
    col = line_color, lty = line_type, lwd = line_width,
    ...)

  # Add the point of the initial estimate.
  points(0, 0, pch = 17, col = "black", cex = 1)
  plot_estimate = ifelse(plot_t, estimate / se, estimate)
  text(0, 0.015,
       paste0("Unadjusted\n(",
              signif(plot_estimate, 2),
              ")"),
       cex = 1)

  # Now plot each of the benchmarks
  if(!is.null(multipliers_y) && !is.null(r2d) && !is.null(r2y)) {
    for(j in seq.int(length(r2y))) {
      plot_benchmark(r2y = r2y[j],
                     r2d = r2d[j],
                     covariate_name = benchmark_covariate[j],
                     multipliers_y = multipliers_y,
                     multipliers_d = multipliers_d,
                     estimate = estimate,
                     se = se,
                     dof = dof,
                     plot_t = plot_t
      )
    }
  }
}

plot_benchmark = function(
  r2y, r2d, covariate_name, multipliers_y, multipliers_d,
  estimate, se, dof, plot_t) {
  for(i in seq.int(length(multipliers_y))) {
    # Get how the effect is reduced under coefficient mutiplier:
    bound = bound_calculator(r2y, r2d,
                             multiplier_y = multipliers_y[i],
                             multiplier_d = multipliers_d[i])

    # Add the point on the contour:
    points(bound$r2_dz, bound$r2_yz,
           pch = 23, col = "black", bg = "red",
           cex = 1, font = 1)

    # Get the attenuation of the effect
    if(!plot_t) {
      adjusted_estimate = estimate - bias_in_r2(r2y = bound$r2_yz,
                                                r2d = bound$r2_dz,
                                                se = se, dof = dof)
    } else {
      adjusted_estimate = t_r2(r2y = bound$r2_yz,
                               r2d = bound$r2_dz,
                               estimate = estimate,
                               se = se, dof = dof)
    }

    # Generate the label text
    variable_text = ifelse(
      is.null(covariate_name),
      "\n",
      paste0(" ", covariate_name, "\n")
    )

    multiplier_text = ifelse(
      multipliers_y[i] == multipliers_d[i],
      paste0(multipliers_y[i], "x"),
      paste0(multipliers_d[i], "/", multipliers_y[i],"x")
    )

    label = paste0(
      multiplier_text, variable_text,
      "(", signif(adjusted_estimate, 2), ")"
    )

    # Add the label text
    text(bound$r2_dz, bound$r2_yz + 0.015,
         labels = label,
         cex = 1.1, font = 1)
  }
}

bound_calculator = function(r2y, r2d,
                             multiplier_y = 1,
                             multiplier_d = 1) {

  if(!is.numeric(multiplier_y) || length(multiplier_y) > 1 ||
     multiplier_y < 0) {
    stop("`multiplier_y` must be a single non-negative number.")
  }
  if(!is.numeric(multiplier_d) || length(multiplier_d) > 1 ||
     multiplier_d < 0) {
    stop("`multiplier_d` must be a single non-negative number.")
  }

  # Bound for R^2 of D with Z: Footnote 10 to reparam f^2 as partial r^2
  # Then eqn. 54 from appendix
  r2_dz = multiplier_d * (r2d / (1 - r2d))

  # Bound for R^2 of Y with Z: Footnote 10
  r2_zx = multiplier_d * (r2d^2) / ((1 - multiplier_d * r2d) * (1 - r2d))

  if(r2_zx > 1) {
    stop("Error: negative square root in r2_yz calculation. ",
         "r2_zx = ", round(r2_zx, 3), " > 1, forcing r2_yz to ",
         "contain negative square root. Try a lower `multiplier_d`")
  }

  r2_yz = ((sqrt(multiplier_y) + sqrt(r2_zx)) /
         sqrt(1 - r2_zx))^2 * (r2y / (1 - r2y))

  list(r2_dz = r2_dz,
       r2_yz = r2_yz)
}

error_estimate = function(estimate) {
  # Error if we don't have an estimate or if it's non-numeric
  if(is.null(estimate)) {
    stop("You must supply either a `model` and `treatment_covariate` to ",
         "extract an estimate or a directly supplied `estimate` argument")
  }
  if(!is.numeric(estimate)) {
    stop("The estimated effect must be numeric.")
  }
}

error_r2 = function(r2d, r2y) {
  # Check r2d / r2y
  if(is.null(r2d) != is.null(r2y)) {
    stop("Either both `r2d` and `r2y` must be provided or neither.")
  }

  if(!is.null(r2d)) {
    if(any(!is.numeric(r2d) | r2d < 0 | r2d > 1)) {
      stop("`r2d` must be a number between zero and one, if provided")
    }

    if(length(r2d) != length(r2y)) {
      stop("Lengths of `r2d` and `r2y` must match.")
    }
  }

  if(!is.null(r2y) &&
     any(!is.numeric(r2y) | r2y < 0 | r2y > 1)) {
    stop("`r2y` must be a number between zero and one, if provided")
  }
}

error_limit = function(lim) {
  # Error if our grid limit is wrong.
  if(any(!is.numeric(lim) | lim < 0 | lim > 1) ||
     lim[2] <= lim[1] ||
     length(lim) != 3) {
    stop("Plot grid must consist of three numbers: lim = c(start, end, by)")
  }
}

error_multipliers = function(multipliers_y, multipliers_d) {
  # Error if multipliers are wrong.
  if(any(!is.numeric(multipliers_y)) || any(!is.numeric(multipliers_d)) ||
     any(multipliers_y < 0) || any(multipliers_d < 0)) {
    stop("`multipliers_y` and `multipliers_d` must be vectors of non-negative ",
         "numbers")
  }
  if(length(multipliers_y) != length(multipliers_d)) {
    stop("`multipliers_y` and `multipliers_d` must be the same length.")
  }
}
