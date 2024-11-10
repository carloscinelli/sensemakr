
# Plots -------------------------------------------------------------------

# keep track of plot options
# to add bound to contours automatically, we need to recover:
#  1. the lims used
#  2. what the sensitivity was
plot.env <- new.env(parent = emptyenv())
plot.env$lim.x <- NULL
plot.env$lim.y <- NULL
plot.env$reduce <- NULL
plot.env$sensitivity.of <- NULL
plot.env$treatment <- NULL

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
#' @export
plot.sensemakr = function(x,
                          type = c("contour", "extreme"),
                          ...) {

  type <- match.arg(type)

  # Call the dispatch function of interest
  plotter <- switch(type,
                    "contour" = ovb_contour_plot,
                    "extreme" = ovb_extreme_plot)
  plotter(x, ...)
}


#' @export
ovb_contour_plot.sensemakr <- function(x,
                                       sensitivity.of = c("estimate", "t-value", "lwr", "upr"), ...){

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

  estimate <- x$sensitivity_stats$estimate
  q <- x$info$q
  reduce <- x$info$reduce
  alpha <- x$info$alpha
  dof <- x$sensitivity_stats$dof
  thr <- ifelse(reduce, estimate*(1 - q), estimate*(1 + q) )
  t.thr <- abs(qt(alpha/2, df = dof - 1))*sign(x$sensitivity_stats$t_statistic)
  plot.env$treatment <- x$info$treatment

  with(x,
       ovb_contour_plot(estimate = sensitivity_stats$estimate,
                        se = sensitivity_stats$se,
                        dof = sensitivity_stats$dof,
                        r2dz.x = r2dz.x,
                        r2yz.dx = r2yz.dx,
                        bound_label = bound_label,
                        sensitivity.of = sensitivity.of,
                        reduce = reduce,
                        estimate.threshold = thr,
                        t.threshold = t.thr,
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
  estimate <- x$sensitivity_stats$estimate
  q <- x$info$q
  reduce <- x$info$reduce
  thr <- ifelse(reduce, estimate*(1 - q), estimate*(1 + q) )
  with(x,
       ovb_extreme_plot(estimate = sensitivity_stats$estimate,
                        se = sensitivity_stats$se,
                        dof = sensitivity_stats$dof,
                        r2dz.x = r2dz.x,
                        r2yz.dx = r2yz.dx,
                        reduce = reduce,
                        threshold = thr,
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
#'  The dotted red line show the chosen critical threshold (for instance, zero): confounders with such strength (or stronger) are sufficient to invalidate the research conclusions.
#'  All results are exact for single confounders and conservative for multiple/nonlinear confounders.
#'
#'  See Cinelli and Hazlett (2020) for details.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{\link{lm}} model with the
#' outcome regression, a \code{\link{formula}} describing the model along
#' with the \code{\link{data.frame}} containing the variables of the model,
#' or a numeric vector with the coefficient estimate.
#'
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
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
#' @param sensitivity.of should the contour plot show adjusted estimates (\code{"estimate"}),
#'  adjusted t-values (\code{"t-value"}), adjusted lower limits (\code{"lwr"})
#'  or upper limits (\code{"upr}) of confidence intervals?
#' @param estimate.threshold critical threshold for the point estimate.
#' @param alpha significance level
#' @param t.threshold critical threshold for the t-value. If \code{NULL}, the value of \code{alpha} is used.
#' @param lim sets limit for x-axis. If `NULL`, limits are computed automatically.
#' @param lim.y  sets limit for y-axis. If `NULL`, limits are computed automatically.
#' @param nlevels number of levels for the contour plot.
#' @param col.contour color of contour lines.
#' @param col.thr.line color of threshold contour line.
#' @param label.text should label texts be plotted? Default is \code{TRUE}.
#' @param label.bump.x bump on the x coordinate of label text.
#' @param label.bump.y bump on the y coordinate of label text.
#' @param round number of digits to show in contours and bound values
#' @export
ovb_contour_plot.lm = function(model,
                               treatment,
                               benchmark_covariates = NULL,
                               kd = 1,
                               ky = kd,
                               r2dz.x = NULL,
                               r2yz.dx = r2dz.x,
                               bound_label = "manual",
                               sensitivity.of = c("estimate", "t-value", "lwr", "upr"),
                               reduce = TRUE,
                               estimate.threshold = 0,
                               alpha = 0.05,
                               t.threshold = NULL,
                               nlevels = 10,
                               col.contour = "grey40",
                               col.thr.line = "red",
                               label.text = TRUE,
                               cex.label.text = .7,
                               round = 3,
                               ...) {


  check_multipliers(ky = ky, kd = kd)



  sensitivity.of <- match.arg(sensitivity.of)
  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  model_data <- model_helper.lm(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  if (is.null( t.threshold)) {
    t.threshold <- qt(1 - alpha/2, df = dof)*sign(estimate)
  }

  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    bounds <-  data.frame(r2dz.x = r2dz.x,
                          r2yz.dx = r2yz.dx,
                          bound_label = bound_label,
                          stringsAsFactors = FALSE)
  } else{
    bounds <-  NULL
  }

  if (!is.null(benchmark_covariates)) {

    # we will need to add an option for the bound type
    bench_bounds <- ovb_bounds.lm(model = model,
                                  treatment = treatment,
                                  benchmark_covariates = benchmark_covariates,
                                  kd = kd,
                                  ky = ky,
                                  adjusted_estimates = FALSE)
    bounds <- rbind(bounds, bench_bounds)
  }

  # update treatment env
  plot.env$treatment <- treatment

  ovb_contour_plot(estimate = estimate,
                   se = se,
                   dof = dof,
                   reduce = reduce,
                   estimate.threshold = estimate.threshold,
                   r2dz.x = bounds$r2dz.x,
                   r2yz.dx = bounds$r2yz.dx,
                   bound_label = bounds$bound_label,
                   sensitivity.of = sensitivity.of,
                   alpha = alpha,
                   t.threshold = t.threshold,
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   cex.label.text = cex.label.text,
                   round = round,
                   ...)
}

#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @param sensitivity.of should the contour plot show adjusted estimates (\code{"estimate"})
#' or adjusted t-values (\code{"t-value"})?
#' @param estimate.threshold critical threshold for the point estimate.
#' @param t.threshold critical threshold for the t-value.
#' @param lim sets limit for both axis. If `NULL`, limits are computed automatically.
#' @param lim.x  sets limit for x-axis. If `NULL`, limits are computed automatically.
#' @param lim.y  sets limit for y-axis. If `NULL`, limits are computed automatically.
#' @param nlevels number of levels for the contour plot.
#' @param col.contour color of contour lines.
#' @param col.thr.line color of threshold contour line.
#' @param label.text should label texts be plotted? Default is \code{TRUE}.
#' @param label.bump.x bump on the x coordinate of label text.
#' @param label.bump.y bump on the y coordinate of label text.
#' @param round number of digits to show in contours and bound values
#' @export
ovb_contour_plot.fixest = function(model,
                                   treatment,
                                   benchmark_covariates = NULL,
                                   kd = 1,
                                   ky = kd,
                                   r2dz.x = NULL,
                                   r2yz.dx = r2dz.x,
                                   bound_label = "manual",
                                   sensitivity.of = c("estimate", "t-value", "lwr", "upr"),
                                   reduce = TRUE,
                                   estimate.threshold = 0,
                                   alpha = 0.05,
                                   t.threshold = NULL,
                                   nlevels = 10,
                                   col.contour = "grey40",
                                   col.thr.line = "red",
                                   label.text = TRUE,
                                   cex.label.text = .7,
                                   round = 3,
                                   ...) {


  check_multipliers(ky = ky, kd = kd)


  sensitivity.of <- match.arg(sensitivity.of)
  # extract model data
  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  model_data <- model_helper.fixest(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof
  if (is.null( t.threshold)) {
    t.threshold <- qt(1 - alpha/2, df = dof)
  }

  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    bounds <-  data.frame(r2dz.x = r2dz.x,
                          r2yz.dx = r2yz.dx,
                          bound_label = bound_label,
                          stringsAsFactors = FALSE)
  } else{
    bounds <-  NULL
  }

  if (!is.null(benchmark_covariates)) {

    # we will need to add an option for the bound type
    bench_bounds <- ovb_bounds.fixest(model = model,
                               treatment = treatment,
                               benchmark_covariates = benchmark_covariates,
                               kd = kd,
                               ky = ky,
                               adjusted_estimates = FALSE)
    bounds <- rbind(bounds, bench_bounds)
  }

  # update treatment env
  plot.env$treatment <- treatment

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
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   cex.label.text = cex.label.text,
                   round = round,
                   ...)


}


#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @export
ovb_contour_plot.formula = function(formula,
                                    method = c("lm", "feols"),
                                    vcov = "iid",
                                    data,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    ky = kd,
                                    r2dz.x = NULL,
                                    r2yz.dx = r2dz.x,
                                    bound_label = NULL,
                                    sensitivity.of = c("estimate", "t-value", "lwr", "upr"),
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    alpha = 0.05,
                                    t.threshold = NULL,
                                    nlevels = 10,
                                    col.contour = "grey40",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    cex.label.text = .7,
                                    round = 3,
                                    ...) {

  check_formula(treatment = treatment,
                formula = formula,
                data = data)

  check_multipliers(ky = ky,
                    kd = kd)

  sensitivity.of <- match.arg(sensitivity.of)

  type <- match.arg(method, method)

  if(type == "lm") {
    reg.call <- call(type, formula = substitute(formula), data = substitute(data))
    outcome_model = eval(reg.call)
  } else if(type == "feols") {
    if (!requireNamespace("fixest")) {
      stop("Please install the fixest package.")
    }
    vcov <- vcov
    outcome_model <- fixest::feols(fml = formula, data = data, vcov = vcov)
  }

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
                   nlevels = nlevels,
                   col.contour = col.contour,
                   col.thr.line = col.thr.line,
                   label.text = label.text,
                   cex.label.text = cex.label.text,
                   round = round,
                   ...)
}


#' @inheritParams adjusted_estimate
#' @rdname ovb_contour_plot
#' @importFrom graphics contour points text
#' @importFrom stats coef
#' @param cex.label.text size of the label text.
#' @param xlab label of x axis. If `NULL`, default label is used.
#' @param ylab label of y axis. If `NULL`, default label is used.
#' @param list.par  arguments to be passed to \code{\link{par}}. It needs to be a named list.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param asp the y/x aspect ratio. Default is 1.
#' @param show.unadjusted should the unadjusted estimates be shown? Default is `TRUE`.
#' @param label.unadjusted label for the unadjusted estimate.
#' @export
ovb_contour_plot.numeric = function(estimate,
                                    se,
                                    dof,
                                    r2dz.x = NULL,
                                    r2yz.dx = r2dz.x,
                                    bound_label = rep("manual", length(r2dz.x)),
                                    sensitivity.of = c("estimate", "t-value", "lwr", "upr"),
                                    reduce = TRUE,
                                    estimate.threshold = 0,
                                    alpha = 0.05,
                                    t.threshold = NULL,
                                    show.unadjusted = TRUE,
                                    label.unadjusted = "Observed",
                                    lim = NULL,
                                    lim.x = lim,
                                    lim.y = lim,
                                    nlevels = 10,
                                    col.contour = "black",
                                    col.thr.line = "red",
                                    label.text = TRUE,
                                    cex.label.text = .7,
                                    label.bump.x = NULL,
                                    label.bump.y = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    cex.lab = .8,
                                    cex.axis = .8,
                                    cex.main = 1,
                                    asp = lim.x/lim.y,
                                    list.par = list(mar = c(4,4,1,1), pty = "s"),
                                    round = 3,
                                    ...) {

  check_estimate(estimate)
  check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  if (is.null( t.threshold)) {
    t.threshold <- qt(1 - alpha/2, df = dof)
  }

  if (length(r2dz.x) != length(r2yz.dx)) {
    stop("Length of r2dz.x and r2yz.dx partial R2 must match")
  }

  if (is.null(lim.x)){
    lim.x   <- min(max(c(0.4, r2dz.x*1.2)), 1 - 1e-12)
  }

  if (is.null(lim.y)){
    lim.y <- min(max(c(0.4, r2yz.dx*1.2)), 1 - 1e-12)
  }

  if (is.null(label.bump.x)){
    label.bump.x <- lim.x*(1/15)
  }

  if (is.null(label.bump.y)){
    label.bump.y <- lim.y*(1/15)
  }

  if (lim.x > 1) {
    lim.x <- 1 - 1e-12
    warning("Contour limit larger than 1 was set to 1.")
  }

  if (lim.y > 1) {
    lim.y <- 1 - 1e-12
    warning("Contour limit larger than 1 was set to 1.")
  }

  if (lim.x < 0) {
    lim.x <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
  }

  if (lim.y < 0) {
    lim.y <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
  }



  sensitivity.of <- match.arg(sensitivity.of)

  # Set up the grid for the contour plot
  grid_values.x = seq(0, lim.x, by = lim.x/400)
  grid_values.y = seq(0, lim.y, by = lim.y/400)

  # Are we plotting t or bias in r2?
  if (sensitivity.of == "estimate") {
    z_axis = outer(grid_values.x, grid_values.y,
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
    z_axis = outer(grid_values.x, grid_values.y,
                   FUN = "adjusted_t",
                   se = se, dof = dof,
                   estimate = estimate,
                   reduce = reduce,
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

  if (sensitivity.of %in% c("lwr", "upr")) {
    z_axis = outer(grid_values.x, grid_values.y,
                   FUN = "adjusted_ci",
                   which = sensitivity.of,
                   se = se, dof = dof,
                   estimate = estimate,
                   reduce = reduce,
                   alpha = alpha) # we are computing the t-value of H0: tau = estimate.threshold
    threshold = estimate.threshold
    plot_estimate = adjusted_ci(estimate = estimate, se = se, dof = dof, alpha = alpha,
                                which = sensitivity.of,
                                r2dz.x = 0, r2yz.dx = 0)

    if (!is.null(r2dz.x))
      bound_value <- adjusted_ci(estimate = estimate,
                                 se = se,
                                 dof = dof,
                                 r2dz.x = r2dz.x,
                                 r2yz.dx = r2yz.dx,
                                 which = sensitivity.of,
                                 reduce = reduce,
                                 alpha = alpha)

  }

  out = list(r2dz.x = grid_values.x,
             r2yz.dx = grid_values.y,
             value = z_axis)

  # Aesthetic: Override the 0 line; basically, check which contour curve is
  # the zero curve, and override that contour curve with alternate aesthetic
  # characteristics
  default_levels = pretty(range(z_axis), nlevels)

  too_close <- abs(default_levels - threshold) < min(diff(default_levels))*0.25

  line_color = ifelse(too_close,
                      "transparent",
                      col.contour)

  line_type = ifelse(too_close,
                     1, 1)
  line_width = ifelse(too_close,
                      1, 1)

  # Plot contour plot:
  if (is.null(list.par)) {
    oldpar <- par(mar = c(5, 5, 4, 1) + .1)
    on.exit(par(oldpar))
  } else {
    if (!is.list(list.par)) stop("list.par needs to be a named list")
    oldpar <- do.call("par", list.par)
    on.exit(par(oldpar))
  }


  if (is.null(xlab)) {
    xlab <- expression(paste("Partial ", R^2, " of confounder(s) with the treatment"))
  }

  if (is.null(ylab)) {
    ylab <- expression(paste("Partial ", R^2, " of confounder(s) with the outcome"))
  }

  contour(
    grid_values.x, grid_values.y, z_axis,
    nlevels = nlevels,
    xlab = xlab,
    ylab = ylab,
    cex.main = cex.main,
    cex.lab = cex.lab,
    cex.axis = cex.axis,
    asp = asp,
    col = line_color,
    lty = line_type,
    lwd = line_width,
    ...)

  contour(grid_values.x, grid_values.y,
          z_axis,
          level = threshold,
          label = round(threshold, digits = round),
          add = TRUE,
          col = col.thr.line,
          lwd = 2,
          lty = 2,
          cex.main = cex.main,
          cex.lab = cex.lab,
          cex.axis = cex.axis,
          asp = asp,
          ...)

  # Add the point of the initial estimate.
  if(show.unadjusted){
    points(0, 0, pch = 17, col = "black", cex = 1)

    text(0.0 + label.bump.x, 0.00 + label.bump.y,
         paste0(label.unadjusted, "\n(",
                round(plot_estimate, digits = round),
                ")"),
         cex = cex.label.text)
  }

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
                         label.bump.y = label.bump.y,
                         cex.label.text = cex.label.text,
                         round = round)
    out$bounds = data.frame(r2dz.x = r2dz.x,
                            r2yz.dx = r2yz.dx,
                            bound_label = bound_label,
                            stringsAsFactors = FALSE)
  }

  # update plot environment variables
  # for further use with add_bounds_to_contour if needed
  plot.env$lim.x <- lim.x
  plot.env$lim.y <- lim.y
  plot.env$reduce <- reduce
  plot.env$sensitivity.of <- sensitivity.of

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
#' ovb_contour_plot(model = model, treatment = "directlyharmed")
#'
#' # add bound 3/1 times stronger than female
#' add_bound_to_contour(model = model,
#'                      benchmark_covariates = "female",
#'                      kd = 3, ky = 1)
#'
#' # add bound 50/2 times stronger than age
#' add_bound_to_contour(model = model,
#'                      benchmark_covariates = "age",
#'                      kd = 50, ky = 2)
#'
#' @param ... arguments passed to other methods.
#' @return
#' The function adds bounds in an existing contour plot and returns `NULL`.
#'
#' @export
add_bound_to_contour <- function(...){
  UseMethod("add_bound_to_contour")
}



#' @export
add_bound_to_contour.ovb_bounds <- function(bounds,
                                            label.text = TRUE,
                                            bound_label = bounds$bound_label,
                                            bound_value = NULL,
                                            label.bump.x = plot.env$lim.x*(1/15),
                                            label.bump.y = plot.env$lim.y*(1/15),
                                            round = 2,
                                            cex.label.text = .7,
                                            ...){


  if (is.null(plot.env$treatment)) {
    stop("No treatment found. Please draw a contour plot first, or provide the treatment variable name manually.")
  }

  # gets bound from environment
  if (any(!bounds$treatment %in% plot.env$treatment)) {
  warning("Treament variable of bounds (",  bounds$treatment, ") ",
          "differs from the treatment variable of the last contour plot (",
          plot.env$treatment, ").")
}

  if (is.null(bound_value) & !is.null(plot.env$sensitivity.of)) {
    if (plot.env$sensitivity.of == "estimate") {
      bound_value <- bounds$adjusted_estimate
    } else {
      bound_value <- bounds$adjusted_t
    }
  }
  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bound_label,
                       label.text = label.text,
                       cex.label.text = cex.label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}


#' @export
add_bound_to_contour.ovb_partial_r2_bound <- function(bounds,
                                                      label.text = TRUE,
                                                      bound_label = bounds$bound_label,
                                                      bound_value = NULL,
                                                      label.bump.x = plot.env$lim.x*(1/15),
                                                      label.bump.y = plot.env$lim.y*(1/15),
                                                      round = 2,
                                                      cex.label.text = .7,
                                                      ...){
  # gets bound from environment
  #

  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bound_label,
                       label.text = label.text,
                       cex.label.text = cex.label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}

#' @inheritParams ovb_contour_plot
#' @rdname add_bound_to_contour
#' @export
add_bound_to_contour.lm <- function(model,
                                    benchmark_covariates,
                                    kd = 1,
                                    ky = kd,
                                    bound_label = NULL,
                                    treatment = plot.env$treatment,
                                    reduce = plot.env$reduce,
                                    sensitivity.of = plot.env$sensitivity.of,
                                    label.text = TRUE,
                                    cex.label.text = .7,
                                    label.bump.x = plot.env$lim.x*(1/15),
                                    label.bump.y = plot.env$lim.y*(1/15),
                                    round = 2,
                                    ...)
{
  sensitivity.of <- match.arg(sensitivity.of)

  if (is.null(plot.env$treatment)) {
    stop("No treatment found. Please draw a contour plot first, or provide the treatment variable name manually.")
  }

  if (treatment != plot.env$treatment) {
    warning("Treament variable provided (",  treatment, ") ",
            "differs from the treatment variable of the last contour plot (",
            plot.env$treatment, ").")
  }

  # we will need to add an option for the bound type
  bounds <- ovb_bounds.lm(model = model,
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

  if (is.null(bound_label)) {
   bound_label <-  bounds$bound_label
  }

  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bound_label,
                       label.text = label.text,
                       cex.label.text = cex.label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}

#' @inheritParams ovb_contour_plot
#' @rdname add_bound_to_contour
#' @export
add_bound_to_contour.fixest <- function(model,
                                    benchmark_covariates,
                                    kd = 1,
                                    ky = kd,
                                    bound_label = NULL,
                                    treatment = plot.env$treatment,
                                    reduce = plot.env$reduce,
                                    sensitivity.of = plot.env$sensitivity.of,
                                    label.text = TRUE,
                                    cex.label.text = .7,
                                    label.bump.x = plot.env$lim.x*(1/15),
                                    label.bump.y = plot.env$lim.y*(1/15),
                                    round = 2,
                                    ...)
{
  sensitivity.of <- match.arg(sensitivity.of)

  if (is.null(plot.env$treatment)) {
    stop("No treatment found. Please draw a contour plot first, or provide the treatment variable name manually.")
  }

  if (treatment != plot.env$treatment) {
    warning("Treament variable provided (",  treatment, ") ",
            "differs from the treatment variable of the last contour plot (",
            plot.env$treatment, ").")
  }

  # we will need to add an option for the bound type
  bounds <- ovb_bounds.fixest(model = model,
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

  if (is.null(bound_label)) {
    bound_label <-  bounds$bound_label
  }

  add_bound_to_contour(r2dz.x = bounds$r2dz.x,
                       r2yz.dx = bounds$r2yz.dx,
                       bound_value = bound_value,
                       bound_label = bound_label,
                       label.text = label.text,
                       cex.label.text = cex.label.text,
                       label.bump.x = label.bump.x,
                       label.bump.y = label.bump.y,
                       round = round,
                       ...)
}

#' @inheritParams ovb_contour_plot
#' @rdname add_bound_to_contour
#' @param bound_value value to be printed in label bound.
#' @param round integer indicating the number of decimal places to be used for rounding.
#' @param font.label.text font for the label text.
#' @param point.pch plotting character for \code{\link{points}}.
#' @param point.col color code or name for \code{\link{points}}.
#' @param point.bg background (fill) color for \code{\link{points}}.
#' @param point.cex size of \code{\link{points}}.
#' @param point.font font for \code{\link{points}}.
#' @export
add_bound_to_contour.numeric <- function(r2dz.x,
                                         r2yz.dx,
                                         bound_value = NULL,
                                         bound_label = NULL,
                                         label.text = TRUE,
                                         cex.label.text = .7,
                                         font.label.text = 1,
                                         label.bump.x = plot.env$lim.x*(1/15),
                                         label.bump.y = plot.env$lim.y*(1/15),
                                         round = 2,
                                         point.pch = 23,
                                         point.col = "black",
                                         point.bg = "red",
                                         point.cex = 1,
                                         point.font =1,
                                         ...){

  for (i in seq.int(length(r2dz.x))) {


    # Add the point on the contour:
    points(r2dz.x[i], r2yz.dx[i],
           pch = point.pch,
           col = point.col,
           bg = point.bg,
           cex = point.cex,
           font = point.font)
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
           cex = cex.label.text,
           font = font.label.text)
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
#' The dotted red line represent the threshold for the effect estimate deemed to be problematic (for instance, zero).
#'
#' See Cinelli and Hazlett (2020) for details.
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
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
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
                                cex.legend = 0.65,
                                legend.bty = "n",
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


  model_data <- model_helper.lm(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  if (!is.null(benchmark_covariates)) {
      # TODO: We will need to make bound_type an option later
      bounds <- ovb_bounds.lm(model = model,
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

#' @inheritParams adjusted_estimate
#' @rdname ovb_extreme_plot
#' @param threshold estimate threshold.
#' @param legend should legend be plotted? Default is \code{TRUE}.
#' @param cex.legend size of the text for the legend.
#' @export
ovb_extreme_plot.fixest <- function(model,
                                treatment,
                                benchmark_covariates = NULL,
                                kd = 1,
                                r2yz.dx = c(1, 0.75, 0.5),
                                r2dz.x = NULL,
                                reduce = TRUE,
                                threshold = 0,
                                lim = min(c(r2dz.x + 0.1, 0.5)),
                                legend = TRUE,
                                cex.legend = 0.65,
                                legend.bty = "n",
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


  model_data <- model_helper.fixest(model, covariates = treatment)
  estimate <- model_data$estimate
  se <- model_data$se
  dof <- model_data$dof

  if (!is.null(benchmark_covariates)) {
    # TODO: We will need to make bound_type an option later
    bounds <- ovb_bounds.fixest(model = model,
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
                                    method = c("lm", "feols"),
                                    vcov = "iid",
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
                                    cex.legend = 0.65,
                                    legend.bty = "n",
                                    ...) {
  check_formula(treatment = treatment,
                formula = formula,
                data = data)

  type <- match.arg(method, method)

  if(type == "lm") {
    reg.call <- call(type, formula = substitute(formula), data = substitute(data))
    outcome_model = eval(reg.call)
  } else if(type == "feols") {
    if (!requireNamespace("fixest")) {
      stop("Please install the fixest package.")
    }
    vcov <- vcov
    outcome_model <- fixest::feols(fml = formula, data = data, vcov = vcov)
  }

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
#' @importFrom graphics abline lines legend rug plot par
#' @rdname ovb_extreme_plot
#' @param legend.bty legend box. See \code{bty} argument of \link{par}.
#' @param legend.title the legend title. If \code{NULL}, then default legend is used.
#' @param xlab label of x axis. If `NULL`, default label is used.
#' @param ylab label of y axis. If `NULL`, default label is used.
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
                                    legend.title  = NULL,
                                    cex.legend = 0.65,
                                    legend.bty = "n",
                                    xlab = NULL,
                                    ylab = NULL,
                                    cex.lab = .7,
                                    cex.axis = .7,
                                    list.par = list(oma = c(1, 1, 1, 1)),
                                    ...) {

  # if (is.null(lim)) {
  #   if (is.null(r2dz.x)) {
  #     stop("If `lim` is not provided, `r2dz.x` must be to automatically pick a ",
  #          "plot limit.")
  #   }
  #   lim = max(r2dz.x, na.rm = TRUE) + 0.1
  # }
  if (lim >= 1) {
    lim <- 0.99
    warning("Plot limit on the partial R2 of confounder with the treatment was larger than or equal to 1 and thus was set to 0.99.")
  }
  if (lim < 0) {
    lim <- 0.4
    warning("Plot limit less than 0 was set to 0.4.")
  }

  # x-axis values: R^2 of confounder(s) with treatment
  r2d_values = seq(0, lim, by = 0.001)
  out = list()

  if (!is.null(list.par)) {
    if (!is.list(list.par)) stop("list.par needs to be a named list")
    oldpar <- do.call("par", list.par)
    on.exit(par(oldpar))
  }

  # Iterate through scenarios:
  for (i in seq.int(length(r2yz.dx))) {
    y = adjusted_estimate(estimate,
                          r2yz.dx = r2yz.dx[i],
                          r2dz.x = r2d_values,
                          se = se,
                          dof = dof,
                          reduce = reduce)
    # Initial plot
    if (i == 1) {
      if (estimate < 0) {
        ylim = rev(range(y))
      } else {
        ylim = range(y)
      }

      if (is.null(xlab)) {
        xlab <- expression(paste("Partial ", R^2, " of confounder(s) with the treatment"))

      }

      if (is.null(ylab)) {
        ylab <- "Adjusted effect estimate"
      }

      plot(
        r2d_values, y, type = "l", bty = "L",
        ylim = ylim,
        xlab = xlab,
        ylab = ylab,
        cex.lab  = cex.lab,
        cex.axis = cex.axis,
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
  if (!is.null(r2dz.x)) {
    rug(x = r2dz.x, col = "red", lwd = 2)
    out[["bounds"]] <- r2dz.x
  }

  if (is.null(legend.title)) {
    legend.title <- expression(paste("Partial ", R^2, " of confounder(s) with the outcome"))
  }


  if (legend) {
    old.par <- par(fig = c(0, 1, 0, 1),
                   oma = c(0, 0, 2, 2),
                   mar = c(0, 0, 1.5, 0),
                   new = TRUE)
    on.exit(par(old.par))
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

    legend(
      x = "topright",
      inset = c(0,0),
      bty = legend.bty,
      xpd = TRUE,
      lty = c(seq_len(length(r2yz.dx)), 5),
      col = c(rep("black", length(r2yz.dx)),
              "red"),
      # bty = "n",
      legend = paste0(r2yz.dx * 100, "%"),
      ncol = length(r2yz.dx) + 1,
      title = legend.title,
      cex = cex.legend
    )
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

  if (!is.null(r2dz.x)) {
    if (any(!is.numeric(r2dz.x) | r2dz.x < 0 | r2dz.x > 1)) {
      stop("partial R2 must be a number between zero and one")
    }
  }

  if (!is.null(r2yz.dx) &&
     any(!is.numeric(r2yz.dx) | r2yz.dx < 0 | r2yz.dx > 1)) {
    stop("partial R2 must be a number between zero and one, if provided")
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

