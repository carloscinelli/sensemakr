
# Plots -------------------------------------------------------------------


# generic plot function ---------------------------------------------------


#' Sensitivity analysis plots for \code{sensemakr}
#'
#' @description
#'
#' This function provides the contour and extreme scenario sensitivity plots of the sensitivity analysis results
#' obtained with the function \code{\link{sensemakr}}. They are basically dispatchers to the core plot functions \code{\link{ovb_contour_plot}} and
#' \code{\link{ovb_extreme_plot}}.
#'
#'
#' @param x an object of class \code{\link{sensemakr}}.
#' @param type type of sensitivity plot. It can be \code{"contour"}, for contour plots of omitted
#' variable bias as in \code{\link{ovb_contour_plot}}; or, \code{"extreme"} for
#' extreme scenarios plots of omitted variable bias as in \code{\link{ovb_extreme_plot}}.
#' @param ... arguments passed to the plot functions. Check arguments in  \code{\link{ovb_contour_plot}}
#' and \code{\link{ovb_extreme_plot}}.
#' @inheritParams ovb_contour_plot
#' @export
plot.sensemakr = function(x,
                          type = c("contour", "extreme"),
                          ...) {

  type <- match.arg(type)

  # Call the dispatch function of interest
  switch(type,
         "contour" = dispatch_contour,
         "extreme" = dispatch_extreme)(x,
                                       ...)
}

dispatch_contour = function(x, sensitivity.of = c("estimate", "t-value"), digits = 2, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  sensitivity.of <- match.arg(sensitivity.of)
  estimate <- x$sensitivity_stats$estimate
  q <- x$info$q
  reduce <- x$info$reduce
  alpha <- x$info$alpha
  dof <- x$sensitivity_stats$dof
  thr <- round(ifelse(reduce, estimate*(1 - q), estimate*(1 + q) ), digits = digits)
  t.thr <- round(abs(qt(alpha/2, df = dof))*sign(x$sensitivity_stats$t_statistic), digits = digits)
  ovb_contour_plot(x, sensitivity.of = sensitivity.of, estimate.threshold = thr, t.threshold = t.thr,
                   ...)
}

dispatch_extreme = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  ovb_extreme_plot(x, ...)
}


#' @export
ovb_contour_plot.sensemakr <- function(x, sensitivity.of = c("estimate", "t-value"), ...){

  sensitivity.of <- match.arg(sensitivity.of)

  if (is.null(x$bounds)) {
    r2dz.x <- NULL
    r2yz.dx <- NULL
    bound_label = ""
  } else {
    r2dz.x <- x$bounds$r2dz.x
    r2yz.dx <- x$bounds$r2yz.dx
    bound_label = x$bounds$bound_label
  }

  with(x,
       ovb_contour_plot(estimate = sensitivity_stats$estimate,
                        se = sensitivity_stats$se,
                        dof = sensitivity_stats$dof,
                        r2dz.x = r2dz.x,
                        r2yz.dx = r2yz.dx,
                        bound_label = bound_label,
                        sensitivity.of = sensitivity.of,
                        ...)

  )
}

#' @export
ovb_extreme_plot.sensemakr <- function(x, r2yz.dx = c(1, 0.75, 0.5), ...){

  if (is.null(x$bounds)) {
    r2dz.x <- NULL
    bound_label = ""
  } else {
    r2dz.x <- x$bounds$r2dz.x
    bound_label = x$bounds$bound_label
  }

  with(x,
       ovb_extreme_plot(estimate = sensitivity_stats$estimate,
                        se = sensitivity_stats$se,
                        dof = sensitivity_stats$dof,
                        r2dz.x = r2dz.x,
                        r2yz.dx = r2yz.dx,
                        ...)

  )
}

# contour plot ------------------------------------------------------------



#' Contour plots of omitted variable bias
#'
#' @description
#' Contour plots of omitted variable bias for sensitivity analysis. The main inputs are an \code{\link{lm}} model, the treatment variable
#' and the covariates used for benchmarking the strength of unobserved confounding.
#'
#' The horizontal axis of the plot shows hypothetical values of the partial R2 of the unobserved confounder(s) with the treatment.
#'  The vertical axis shows hypothetical values of the partial R2 of the unobserved confounder(s) with the outcome.
#'  The contour levels represent the adjusted estimates (or t-values) of the treatment effect.
#'  The reference points are the bounds on the partial R2 of the unobserved confounder if it were \code{k} times ``as strong'' as the observed covariate used for benchmarking (see arguments \code{kd} and \code{ky}).
#'
#'  See Cinelli and Hazlett (2018) for details.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{\link{lm}} model with the
#' outcome regression, a \code{\link{formula}} describing the model along
#' with the \code{\link{data.frame}} containing the variables of the model,
#' or a numeric vector with the coefficient estimate.
#'
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#'
#' @return
#' The function returns invisibly the data used for the contour plot (contour grid and bounds).
#'
#' @examples
#'
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'                          pastvoted + hhsize_darfur + female + village,
#'                          data = darfur)
#' # contour plot
#' ovb_contour_plot(model, treatment = "directlyharmed",
#'                         benchmark_covariates = "female",
#'                         kd = 1:2)
#'
#' @export
ovb_contour_plot = function(...) {
  UseMethod("ovb_contour_plot")
}





#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @param sensitivity.of should the contour plot show adjusted estimates (\code{"estimate"})
#' or adjusted t-values (\code{"t-value"})?
#' @param estimate.threshold threshold for plot of adjusted estimate.
#' @param t.threshold threshold for plot of adjusted t-value.
#' @param lim sets limit of the plot.
#' @param nlevels number of levels to contour plot.
#' @param col.contour color of contour lines.
#' @param col.thr.line color of threshold contour line.
#' @param label.text should label texts be plotted? Default is \code{TRUE}.
#' @param label.bump.x bump on the x coordinate of label text.
#' @param label.bump.y bump on the y coordinate of label text.
#' @export
ovb_contour_plot.lm = function(model,
                               treatment,
                               benchmark_covariates = NULL,
                               kd = 1,
                               ky = kd,
                               r2dz.x = NULL,
                               r2yz.dx = r2dz.x,
                               bound_label = NULL,
                               sensitivity.of = c("estimate", "t-value"),
                               reduce = TRUE,
                               estimate.threshold = 0,
                               t.threshold = 2,
                               lim = max(c(0.4,r2dz.x,r2yz.dx)),
                               nlevels = 20,
                               col.contour = "grey40",
                               col.thr.line = "red",
                               label.text = TRUE,
                               label.bump.x = 0.02,
                               label.bump.y = 0.02,
                               ...) {


  check_multipliers(ky = ky, kd = kd)
  if (lim > 1) {
    lim <- 1
    warning("Contour limit larger than 1 was set to 1.")
  }
  if (lim < 0) {
    lim <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
  }

  sensitivity.of <- match.arg(sensitivity.of)
  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  model_data <- model_helper(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    bounds <-  data.frame(r2dz.x = r2dz.x,
                          r2yz.dx = r2yz.dx,
                          bound_label = bound_label,
                          stringsAsFactors = FALSE)
    lim <- max(c(lim, r2dz.x, r2yz.dx))
  } else{
    bounds <-  NULL
  }

  if (!is.null(benchmark_covariates)) {

    # we will need to add an option for the bound type
    bench_bounds <- ovb_bounds(model = model,
                               treatment = treatment,
                               benchmark_covariates = benchmark_covariates,
                               kd = kd,
                               ky = ky,
                               adjusted_estimates = FALSE)
    bounds <- rbind(bounds, bench_bounds)
    lim <- max(c(lim, bounds$r2dz.x, bounds$r2yz.dx))
  }

  ovb_contour_plot(estimate = estimate,
                   se = se,
                   dof = dof,
                   reduce = reduce,
                   estimate.threshold = estimate.threshold,
                   r2dz.x = bounds$r2dz.x,
                   r2yz.dx = bounds$r2yz.dx,
                   bound_label = bounds$bound_label,
                   sensitivity.of = sensitivity.of,
                   t.threshold = t.threshold,
                   lim = lim,
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   label.bump.x = label.bump.x,
                   label.bump.y = label.bump.y,
                   ...)


}

#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.formula = function(formula,
                                    data,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    ky = kd,
                                    r2dz.x = NULL,
                                    r2yz.dx = r2dz.x,
                                    bound_label = NULL,
                                    sensitivity.of = c("estimate", "t-value"),
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    t.threshold = 2,
                                    lim = max(c(0.4,r2dz.x,r2yz.dx)),
                                    nlevels = 20,
                                    col.contour = "grey40",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    ...) {

  check_formula(treatment = treatment,
                formula = formula,
                data = data)

  check_multipliers(ky = ky,
                    kd = kd)

  sensitivity.of <- match.arg(sensitivity.of)


  lm.call <- call("lm", formula = substitute(formula), data = substitute(data))
  outcome_model = eval(lm.call)

  ovb_contour_plot(model = outcome_model,
                   treatment = treatment,
                   benchmark_covariates = benchmark_covariates,
                   kd = kd,
                   ky = ky,
                   reduce = reduce,
                   estimate.threshold = estimate.threshold,
                   r2dz.x = r2dz.x,
                   r2yz.dx = r2dz.x,
                   bound_label = bound_label,
                   sensitivity.of = sensitivity.of,
                   t.threshold = t.threshold,
                   lim = lim,
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   ...)
}


#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @importFrom graphics contour points text
#' @importFrom stats coef
#' @export
ovb_contour_plot.numeric = function(estimate,
                                    se,
                                    dof,
                                    r2dz.x = NULL,
                                    r2yz.dx = r2dz.x,
                                    bound_label = "",
                                    sensitivity.of = c("estimate", "t-value"),
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    t.threshold = 2,
                                    lim = max(c(0.4, r2dz.x+0.1,r2yz.dx+0.1)),
                                    nlevels = 20,
                                    col.contour = "grey40",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    label.bump.x = 0.02,
                                    label.bump.y = 0.02,
                                    ...) {

  check_estimate(estimate)
  check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  lim <- check_contour_lim(lim)

  sensitivity.of <- match.arg(sensitivity.of)

  # Set up the grid for the contour plot
  grid_values = seq(0, lim, by = lim/400)

  # Are we plotting t or bias in r2?
  if (sensitivity.of == "estimate") {
    z_axis = outer(grid_values, grid_values,
                   FUN = "adjusted_estimate",
                   estimate = estimate,
                   se = se,
                   dof = dof,
                   reduce = reduce)
    threshold = estimate.threshold
    plot_estimate = estimate

    if (!is.null(r2dz.x))
      bound_value <- adjusted_estimate(estimate = estimate,
                                       se = se,
                                       dof = dof,
                                       r2dz.x = r2dz.x,
                                       r2yz.dx = r2yz.dx,
                                       reduce = reduce)

  }

  if (sensitivity.of == "t-value") {
    z_axis = outer(grid_values, grid_values,
                   FUN = "adjusted_t",
                   se = se, dof = dof, estimate = estimate,
                   h0 = estimate.threshold) # we are computing the t-value of H0: tau = estimate.threshold
    threshold = t.threshold
    plot_estimate = (estimate - estimate.threshold) / se

    if (!is.null(r2dz.x))
      bound_value <- adjusted_t(estimate = estimate,
                                se = se,
                                dof = dof,
                                r2dz.x = r2dz.x,
                                r2yz.dx = r2yz.dx,
                                reduce = reduce, h0 = estimate.threshold)

  }

  out = list(r2dz.x = grid_values,
             r2yz.dx = grid_values,
             value = z_axis)

  # Aesthetic: Override the 0 line; basically, check which contour curve is
  # the zero curve, and override that contour curve with alternate aesthetic
  # characteristics
  default_levels = pretty(range(z_axis), nlevels)
  line_color = ifelse(default_levels == threshold,
                      "transparent",
                      col.contour)
  line_type = ifelse(default_levels == threshold,
                     1, 1)
  line_width = ifelse(default_levels == threshold,
                      1, 1)

  # Plot contour plot:
  oldpar <- par(mar = c(5, 5, 4, 1) + .1)
  contour(
    grid_values, grid_values, z_axis, nlevels = nlevels,
    xlab = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                            " confounder(s) with the treatment")),
    ylab = expression(paste("Hypothetical partial ", R^2, " of unobserved",
                            " confounder(s) with the outcome")),
    cex.main = 1, cex.lab = 1, cex.axis = 1,
    col = line_color, lty = line_type, lwd = line_width,
    ...)
  contour(grid_values, grid_values,
          z_axis, level = threshold,
          add = TRUE,
          col = col.thr.line,
          lwd = 2, lty = 2)

  # Add the point of the initial estimate.
  points(0, 0, pch = 17, col = "black", cex = 1)

  text(0.0 + label.bump.x, 0.00 + label.bump.y,
       paste0("Unadjusted\n(",
              signif(plot_estimate, 2),
              ")"),
       cex = 1)

  # add bounds
  if (!is.null(r2dz.x)) {

    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)


    add_bound_to_contour(r2dz.x = r2dz.x,
                         r2yz.dx = r2yz.dx,
                         bound_value = bound_value,
                         bound_label = bound_label,
                         sensitivity.of = sensitivity.of,
                         label.text = label.text,
                         label.bump.x = label.bump.x,
                         label.bump.y = label.bump.y)
    out$bounds = data.frame(r2dz.x = r2dz.x,
                            r2yz.dx = r2yz.dx,
                            bound_label = bound_label)
  }
  par(oldpar)
  invisible(out)
}



# add bound to contour ----------------------------------------------------


#' Add bounds to contour plot of omitted variable bias
#'
#' @description
#' Convenience function to add bounds on a sensitivity contour plot created with \code{\link{ovb_contour_plot}}.
#'
#' @examples
#'
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'                          pastvoted + hhsize_darfur + female + village,
#'                          data = darfur)
#' # contour plot
#' ovb_contour_plot(model, treatment = "directlyharmed")
#'
#' # add bound 3/1 times stronger than female
#' add_bound_to_contour(model,
#'                      treatment = "directlyharmed",
#'                      benchmark_covariates = "female",
#'                      kd = 3, ky = 1)
#'
#' # add bound 50/2 times stronger than age
#' add_bound_to_contour(model,
#'                      treatment = "directlyharmed",
#'                      benchmark_covariates = "age",
#'                      kd = 50, ky = 2)
#'
#' @param ... arguments passed to other methods.
#' @export
add_bound_to_contour <- function(...){
  UseMethod("add_bound_to_contour")
}

#' @export
add_bound_to_contour.ovb_bounds <- function(bounds,
                                            bound_value = NULL,
                                            label.text = TRUE,
                                            label.bump.x = 0.02,
                                            label.bump.y = 0.02,
                                            round = 2,
                                            ...){
  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bounds$bound_label,
                       label.text = label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}

#' @inheritParams ovb_contour_plot
#' @rdname add_bound_to_contour
#' @export
add_bound_to_contour.lm <- function(model,
                                    treatment,
                                    benchmark_covariates,
                                    kd = 1,
                                    ky = kd,
                                    reduce = TRUE,
                                    sensitivity.of = c("estimate", "t-value"),
                                    label.text = TRUE,
                                    label.bump.x = 0.02,
                                    label.bump.y = 0.02,
                                    round = 2,
                                    ...)
{
  sensitivity.of <- match.arg(sensitivity.of)
  # we will need to add an option for the bound type
  bounds <- ovb_bounds(model = model,
                       treatment = treatment,
                       benchmark_covariates = benchmark_covariates,
                       kd = kd,
                       ky = ky,
                       adjusted_estimates = TRUE,
                       reduce = reduce)

  if (sensitivity.of == "estimate") {
    bound_value <- bounds$adjusted_estimate
  }

  if (sensitivity.of == "t-value") {
    bound_value <- bounds$adjusted_t
  }
  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bounds$bound_label,
                       label.text = label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}

#' @inheritParams ovb_contour_plot
#' @rdname add_bound_to_contour
#' @param bound_value value to be printed in label bound.
#' @param round integer indicating the number of decimal places to be used for rounding.
#' @export
add_bound_to_contour.numeric <- function(r2dz.x,
                                         r2yz.dx,
                                         bound_value = NULL,
                                         bound_label = NULL,
                                         label.text = TRUE,
                                         label.bump.x = 0.02,
                                         label.bump.y = 0.02,
                                         round = 2,
                                         ...){

  for (i in seq.int(length(r2dz.x))) {


    # Add the point on the contour:
    points(r2dz.x[i], r2yz.dx[i],
           pch = 23, col = "black", bg = "red",
           cex = 1, font = 1)
    if (!is.null(bound_value[i])) {
      if (is.numeric(bound_value[i]))
        bound_value[i] <- round(bound_value[i], round)

      label = paste0(bound_label[i], "\n(", bound_value[i], ")")
    } else {
     label = bound_label[i]
    }

    # Add the label text
    if (label.text)
      text(r2dz.x[i] + label.bump.x,
           r2yz.dx[i] + label.bump.y,
           labels = label,
           cex = 1.1, font = 1)
  }

}

# extreme plot ------------------------------------------------------------


#' Extreme scenarios plots of omitted variable bias
#'
#' @description
#' Extreme scenario plots of omitted variable bias for sensitivity analysis. The main inputs are an \code{\link{lm}} model, the treatment variable
#' and the covariates used for benchmarking the strength of unobserved confounding.
#'
#' The horizontal axis shows the partial R2 of the unobserved confounder(s) with the treatment. The vertical axis shows the adjusted treatment effect estimate.
#' The partial R2 of the confounder with the outcome is represented by \emph{different curves} for each scenario, as given by the parameter \code{r2yz.dx}.
#' The red marks on horizontal axis are bounds on the partial R2 of the unobserved confounder \code{kd} times as strong as the covariates used for benchmarking.
#'
#' See Cinelli and Hazlett (2018) for details.
#'
#' @examples
#'
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'                          pastvoted + hhsize_darfur + female + village,
#'                          data = darfur)
#' # extreme scenarios plot
#' ovb_extreme_plot(model, treatment = "directlyharmed",
#'                         benchmark_covariates = "female",
#'                         kd = 1:2,
#'                         lim = 0.05)
#'
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#'
#' @return
#' The function returns invisibly the data used for the extreme plot.
#'
#' @inheritParams ovb_contour_plot
#' @export
ovb_extreme_plot <- function(...){
  UseMethod("ovb_extreme_plot")

}


#' @inheritParams adjusted_estimate
#' @rdname ovb_extreme_plot
#' @param threshold estimate threshold.
#' @param legend should legend be plotted? Default is \code{TRUE}.
#' @param cex.legend size of the text for the legend.
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
                                legend = TRUE,
                                cex.legend = 0.5,
                                legend.bty = "o",
                                ...){

  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  if (lim > 1) {
    lim <- 1
    warning("Plot limit larger than 1 was set to 1.")
  }
  if (lim < 0) {
    lim <- 0.4
    warning("Plot limit less than 0 was set to 0.4.")
  }


  model_data <- model_helper(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  if (!is.null(benchmark_covariates)) {
      # TODO: We will need to make bound_type an option later
      bounds <- ovb_bounds(model = model,
                           treatment = treatment,
                           benchmark_covariates = benchmark_covariates,
                           kd = kd,
                           ky = 1)

      r2dz.x <- c(r2dz.x, bounds$r2dz.x)

  }


  ovb_extreme_plot(estimate = estimate,
                   se = se,
                   dof = dof,
                   r2dz.x = r2dz.x,
                   r2yz.dx = r2yz.dx,
                   reduce = reduce,
                   threshold = threshold,
                   lim = lim,
                   legend = legend,
                   cex.legend = cex.legend,
                   legend.bty = legend.bty,
                   ...)



}


#' @inheritParams sensemakr
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
                                    legend = TRUE,
                                    cex.legend = 0.5,
                                    legend.bty = "o",
                                    ...) {
  check_formula(treatment = treatment,
                formula = formula,
                data = data)

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
                   legend = legend,
                   cex.legend = cex.legend,
                   legend.bty = legend.bty,
                   ...)
}



#' @inheritParams adjusted_estimate
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
                                    legend = TRUE,
                                    cex.legend = 0.5,
                                    legend.bty = "o",
                                    ...) {

  # if (is.null(lim)) {
  #   if (is.null(r2dz.x)) {
  #     stop("If `lim` is not provided, `r2dz.x` must be to automatically pick a ",
  #          "plot limit.")
  #   }
  #   lim = max(r2dz.x, na.rm = TRUE) + 0.1
  # }
  if (lim > 1) {
    lim <- 1
    warning("Plot limit larger than 1 was set to 1.")
  }
  if (lim < 0) {
    lim <- 0.4
    warning("Plot limit less than 0 was set to 0.4.")
  }

  # x-axis values: R^2 of confounder(s) with treatment
  r2d_values = seq(0, lim, by = 0.001)
  out = list()
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
      lines(r2d_values, y, lty = i)
    }
    out[[paste0("scenario_r2yz.dx_",r2yz.dx[i])]] <- data.frame(r2dz.x = r2d_values,
                                                                r2yz.dx = r2yz.dx[i],
                                                                adjusted_estimate = y)

  }

  if (legend) {
    legend(
      x = "topright",
      inset = 0.05,
      bty = legend.bty,
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
  }

  if (!is.null(r2dz.x)) {
    rug(x = r2dz.x, col = "red", lwd = 2)
    out[["bounds"]] <- r2dz.x
  }
  return(invisible(out))
}


# sanity checkers ---------------------------------------------------------


check_formula <- function(treatment, formula, data) {

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

}



check_estimate = function(estimate) {
  # Error if we don't have an estimate or if it's non-numeric
  if (is.null(estimate)) {
    stop("You must supply either a `model` and `treatment_covariate` to ",
         "extract an estimate or a directly supplied `estimate` argument")
  }
  if (!is.numeric(estimate)) {
    stop("The estimated effect must be numeric.")
  }
}

check_r2 = function(r2dz.x, r2yz.dx) {
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

check_multipliers = function(ky, kd) {
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


check_contour_lim <- function(lim) {

  if (lim > 1) {
    lim <- 1
    warning("Contour limit larger than 1 was set to 1.")
    return(lim)
  }

  if (lim < 0) {
    lim <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
    return(lim)
  }

  return(lim)
}
