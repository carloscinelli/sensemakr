#' Sensemakr: extending omitted variable bias
#'
#' The sensemakr package implements a suite of sensitivity analysis tools that makes it easier to
#' understand the impact of omitted variables in linear regression models, as discussed in Cinelli and Hazlett (2020).
#'
#' The main function of the package is \code{\link{sensemakr}}, which computes the most common sensitivity analysis results.
#' After running \code{sensemakr} you may directly use the plot and print methods in the returned object.
#'
#'  You may also use the other sensitivity functions of the package directly, such as the functions for sensitivity plots
#' (\code{\link{ovb_contour_plot}}, \code{\link{ovb_extreme_plot}}) the functions for computing bias-adjusted estimates and t-values (\code{\link{adjusted_estimate}}, \code{\link{adjusted_t}}),
#' the functions for computing the robustness value and partial R2 (\code{\link{robustness_value}}, \code{\link{partial_r2}}),  or the functions for bounding the strength
#' of unobserved confounders (\code{\link{ovb_bounds}}), among others.
#'
#' More information can be found on the help documentation, vignettes and related papers.
#'
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @docType package
#' @examples
#'
#' # loads dataset
#' data("darfur")
#'
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'               pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' # runs sensemakr for sensitivity analysis
#' sensitivity <- sensemakr(model, treatment = "directlyharmed",
#'                          benchmark_covariates = "female",
#'                          kd = 1:3)
#' # short description of results
#' sensitivity
#'
#' # long description of results
#' summary(sensitivity)
#'
#' # plot bias contour of point estimate
#' plot(sensitivity)
#'
#' # plot bias contour of t-value
#' plot(sensitivity, sensitivity.of = "t-value")
#'
#' # plot extreme scenario
#' plot(sensitivity, type = "extreme")
#'
#' # latex code for sensitivity table
#' ovb_minimal_reporting(sensitivity)
#'
#' # data.frame with sensitivity statistics
#' sensitivity$sensitivity_stats
#'
#' #  data.frame with bounds on the strengh of confounders
#' sensitivity$bounds
#'
#' ### Using sensitivity functions directly ###
#'
#' # robustness value of directly harmed q = 1 (reduce estimate to zero)
#' robustness_value(model, covariates = "directlyharmed")
#'
#' # robustness value of directly harmed q = 1/2 (reduce estimate in half)
#' robustness_value(model, covariates = "directlyharmed", q = 1/2)
#'
#' # robustness value of directly harmed q = 1/2, alpha = 0.05
#' robustness_value(model, covariates = "directlyharmed", q = 1/2, alpha = 0.05)
#'
#' # partial R2 of directly harmed with peacefactor
#' partial_r2(model, covariates = "directlyharmed")
#'
#' # partial R2 of female with peacefactor
#' partial_r2(model, covariates = "female")
#'
#' # data.frame with sensitivity statistics
#' sensitivity_stats(model, treatment = "directlyharmed")
#'
#' # bounds on the strength of confounders using female and age
#' ovb_bounds(model,
#'            treatment = "directlyharmed",
#'            benchmark_covariates = c("female", "age"),
#'            kd = 1:3)
#'
#' # adjusted estimate given hypothetical strength of confounder
#' adjusted_estimate(model, treatment = "directlyharmed", r2dz.x = 0.1, r2yz.dx = 0.1)
#'
#' # adjusted t-value given hypothetical strength of confounder
#' adjusted_t(model, treatment = "directlyharmed", r2dz.x = 0.1, r2yz.dx = 0.1)
#'
#' # bias contour plot directly from lm model
#' ovb_contour_plot(model,
#'                  treatment = "directlyharmed",
#'                  benchmark_covariates = "female",
#'                  kd = 1:3)
#'
#' # extreme scenario plot directly from lm model
#' ovb_extreme_plot(model,
#'                  treatment = "directlyharmed",
#'                  benchmark_covariates = "female",
#'                  kd = 1:3, lim = 0.05)
#' @name sensemakr-package
"_PACKAGE"

#' Sensitivity analysis to unobserved confounders
#'
#' @description
#' This function performs sensitivity analysis to omitted variables as discussed in Cinelli and Hazlett (2020). It returns an object of
#' class \code{sensemakr} with several pre-computed sensitivity statistics for reporting.
#' After running \code{sensemakr} you may directly use the \code{plot}, \code{print} and \code{summary} methods in the returned object.
#'
#' @seealso
#'
#' The function \code{sensemakr} is a convenience function. You may use the other sensitivity functions of the package directly, such as the functions for sensitivity plots
#' (\code{\link{ovb_contour_plot}}, \code{\link{ovb_extreme_plot}}) the functions for computing bias-adjusted estimates and t-values (\code{\link{adjusted_estimate}}, \code{\link{adjusted_t}}),
#' the functions for computing the robustness value and partial R2 (\code{\link{robustness_value}}, \code{\link{partial_r2}}),  or the functions for bounding the strength
#' of unobserved confounders (\code{\link{ovb_bounds}}), among others.
#'
#' @param ... arguments passed to other methods. First argument should either be (i) an \code{\link{lm}} or \code{\link[fixest]{feols}} model with the
#' outcome regression (argument \code{model}); (ii) a \code{\link{formula}} describing the model along
#' with the \code{\link{data.frame}} containing the variables of the model (arguments \code{formula} and \code{data}); or, (iii) the numerical estimated value of the coefficient, along with the numeric values of standard errors and degrees of freedom (arguments \code{estimate}, \code{se} and \code{df}).
#'
#'
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @examples
#' # loads dataset
#' data("darfur")
#'
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'                          pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' # runs sensemakr for sensitivity analysis
#'sensitivity <- sensemakr(model, treatment = "directlyharmed",
#'                                benchmark_covariates = "female",
#'                                kd = 1:3)
#'# short description of results
#'sensitivity
#'
#'# long description of results
#'summary(sensitivity)
#'
#'# plot bias contour of point estimate
#'plot(sensitivity)
#'
#'# plot bias contour of t-value
#'plot(sensitivity, sensitivity.of = "t-value")
#'
#'# plot bias contour of lower limit of CI
#'plot(sensitivity, sensitivity.of = "lwr")
#'
#'# plot bias contour of upper limit of CI
#'plot(sensitivity, sensitivity.of = "upr")
#'
#'# plot extreme scenario
#'plot(sensitivity, type = "extreme")
#'
#' # latex code for sensitivity table
#' ovb_minimal_reporting(sensitivity)
#'
#' @return
#' An object of class \code{sensemakr}, containing:
#' \describe{
#'  \item{ \code{info} }{A \code{data.frame} with the general information of the analysis, including the formula used, the name of the treatment variable, parameter values such as \code{q}, \code{alpha}, and whether the bias is assumed to reduce the current estimate. }
#'  \item{ \code{sensitivity_stats} }{A \code{data.frame} with the sensitivity statistics for the treatment variable, as computed by the function \code{\link{sensitivity_stats}}.}
#'  \item{ \code{bounds} }{A \code{data.frame} with bounds on the strength of confounding according to some benchmark covariates, as computed by the function \code{\link{ovb_bounds}}.}
#'  }
#'
#' @export
sensemakr <- function(...){
  UseMethod("sensemakr")
}

#' @export
#' @inheritParams adjusted_estimate
#' @param benchmark_covariates The user has two options: (i)  character vector of the names of covariates that will be used to bound the plausible strength of the unobserved confounders. Each variable will be considered separately; (ii) a named list with character vector names of covariates that will be used, \emph{as a group}, to bound the plausible strength of the unobserved confounders. The names of the list will be used for the benchmark labels. Note: for factor variables with more than two levels, you need to provide the name of each level as encoded in the \code{lm} model (the columns of \code{model.matrix}).
#' @param kd numeric vector. Parameterizes how many times stronger the confounder is related to the treatment in comparison to the observed benchmark covariate.
#' Default value is \code{1} (confounder is as strong as benchmark covariate).
#' @param ky numeric vector. Parameterizes how many times stronger the confounder is related to the outcome in comparison to the observed benchmark covariate.
#' Default value is the same as \code{kd}.
#' @param bound_label label to bounds provided manually in \code{r2dz.x} and \code{r2yz.dx}.
#' @inheritParams robustness_value
#' @rdname sensemakr
#' @importFrom stats formula
sensemakr.lm <- function(model,
                         treatment,
                         benchmark_covariates = NULL,
                         kd = 1,
                         ky = kd,
                         q = 1,
                         alpha = 0.05,
                         r2dz.x = NULL,
                         r2yz.dx = r2dz.x,
                         bound_label = "Manual Bound",
                         reduce = TRUE,
                         ...){
  out <- list()
  out$info <- list(formula = formula(model),
                   treatment = treatment,
                   q = q,
                   alpha = alpha,
                   reduce = reduce)



  # senstivity statistics
  out$sensitivity_stats <- sensitivity_stats.lm(model = model,
                                                treatment = treatment,
                                                q = q,
                                                alpha = alpha,
                                                reduce = reduce)
  estimate <- out$sensitivity_stats$estimate

  h0 <- ifelse(reduce, estimate*(1 - q), estimate*(1 + q))

  # bounds on ovb
  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    out$bounds <-  data.frame(r2dz.x = r2dz.x,
                              r2yz.dx = r2yz.dx,
                              bound_label = bound_label,
                              stringsAsFactors = FALSE)

    out$bounds$treatment <- treatment

    # compute adjusted effects
    out$bounds$adjusted_estimate = adjusted_estimate.lm(model = model,
                                                        treatment = treatment,
                                                        r2yz.dx = r2yz.dx,
                                                        r2dz.x = r2dz.x,
                                                        reduce = reduce)

    out$bounds$adjusted_se = adjusted_se.lm(model = model,
                                            treatment = treatment,
                                            r2yz.dx = r2yz.dx,
                                            r2dz.x = r2dz.x)

    out$bounds$adjusted_t = adjusted_t.lm(model = model,
                                          treatment = treatment,
                                          r2yz.dx = r2yz.dx,
                                          r2dz.x = r2dz.x,
                                          h0 = h0,
                                          reduce = reduce)

    se_multiple <- qt(alpha/2, df = model$df.residual, lower.tail = F)
    out$bounds$adjusted_lower_CI <- out$bounds$adjusted_estimate - se_multiple*out$bounds$adjusted_se
    out$bounds$adjusted_upper_CI <- out$bounds$adjusted_estimate + se_multiple*out$bounds$adjusted_se
  } else{
    out$bounds <-  NULL
  }

  if (!is.null(benchmark_covariates)) {
    bench_bounds <- ovb_bounds.lm(model = model,
                                  treatment = treatment,
                                  benchmark_covariates = benchmark_covariates,
                                  kd = kd,
                                  ky = ky,
                                  q = q,
                                  alpha = alpha,
                                  h0 = h0,
                                  reduce = reduce)
    out$bounds <- rbind(out$bounds, bench_bounds)
  }

  class(out) <- "sensemakr"

  return(out)
}

#' @export
#' @inheritParams adjusted_estimate
#' @param benchmark_covariates The user has two options: (i)  character vector of the names of covariates that will be used to bound the plausible strength of the unobserved confounders. Each variable will be considered separately; (ii) a named list with character vector names of covariates that will be used, \emph{as a group}, to bound the plausible strength of the unobserved confounders. The names of the list will be used for the benchmark labels. Note: for factor variables with more than two levels, you need to provide the name of each level as encoded in the \code{fixest} model (the columns of \code{model.matrix}).
#' @param kd numeric vector. Parameterizes how many times stronger the confounder is related to the treatment in comparison to the observed benchmark covariate.
#' Default value is \code{1} (confounder is as strong as benchmark covariate).
#' @param ky numeric vector. Parameterizes how many times stronger the confounder is related to the outcome in comparison to the observed benchmark covariate.
#' Default value is the same as \code{kd}.
#' @param bound_label label to bounds provided manually in \code{r2dz.x} and \code{r2yz.dx}.
#' @param ... arguments passed to other methods.
#' @inheritParams robustness_value
#' @rdname sensemakr
#' @importFrom stats formula
sensemakr.fixest <- function(model,
                             treatment,
                             benchmark_covariates = NULL,
                             kd = 1,
                             ky = kd,
                             q = 1,
                             alpha = 0.05,
                             r2dz.x = NULL,
                             r2yz.dx = r2dz.x,
                             bound_label = "Manual Bound",
                             reduce = TRUE,
                             ...){

  if(model$method != "feols") {
    stop("The fixest method is only implemented for feols")
  }

  message_vcov.fixest(model)

  out <- list()
  out$info <- list(formula = formula(model),
                   treatment = treatment,
                   q = q,
                   alpha = alpha,
                   reduce = reduce)



  # senstivity statistics
  out$sensitivity_stats <- sensitivity_stats.fixest(model = model,
                                                    treatment = treatment,
                                                    q = q,
                                                    alpha = alpha,
                                                    reduce = reduce,
                                                    message = F)
  estimate <- out$sensitivity_stats$estimate

  h0 <- ifelse(reduce, estimate*(1 - q), estimate*(1 + q))

  # bounds on ovb
  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    out$bounds <-  data.frame(r2dz.x = r2dz.x,
                              r2yz.dx = r2yz.dx,
                              bound_label = bound_label,
                              stringsAsFactors = FALSE)

    out$bounds$treatment <- treatment

    # compute adjusted effects
    out$bounds$adjusted_estimate = adjusted_estimate.fixest(model = model,
                                                        treatment = treatment,
                                                        r2yz.dx = r2yz.dx,
                                                        r2dz.x = r2dz.x,
                                                        reduce = reduce)

    out$bounds$adjusted_se = adjusted_se.fixest(model = model,
                                            treatment = treatment,
                                            r2yz.dx = r2yz.dx,
                                            r2dz.x = r2dz.x, message = F)

    out$bounds$adjusted_t = adjusted_t.fixest(model = model,
                                          treatment = treatment,
                                          r2yz.dx = r2yz.dx,
                                          r2dz.x = r2dz.x,
                                          h0 = h0,
                                          reduce = reduce, message = F)

    se_multiple <- qt(alpha/2, df = fixest::degrees_freedom(model, type = "resid", vcov = "iid"), lower.tail = F)
    out$bounds$adjusted_lower_CI <- out$bounds$adjusted_estimate - se_multiple*out$bounds$adjusted_se
    out$bounds$adjusted_upper_CI <- out$bounds$adjusted_estimate + se_multiple*out$bounds$adjusted_se
  } else{
    out$bounds <-  NULL
  }

  if (!is.null(benchmark_covariates)) {
    bench_bounds <- ovb_bounds.fixest(model = model,
                                  treatment = treatment,
                                  benchmark_covariates = benchmark_covariates,
                                  kd = kd,
                                  ky = ky,
                                  q = q,
                                  alpha = alpha,
                                  h0 = h0,
                                  reduce = reduce,
                                  message = F)
    out$bounds <- rbind(out$bounds, bench_bounds)
  }

  class(out) <- "sensemakr"

  return(out)
}


#' @param formula an object of the class \code{\link{formula}}: a symbolic description of the model to be fitted.
#' @param method the default is \code{\link{lm}}. This argument can be changed to estimate the model using \code{\link[fixest]{feols}}. In this case the formula needs to be written so it can be estimated with \code{\link[fixest]{feols}} and the package needs to be installed.
#' @param data data needed only when you pass a formula as first parameter. An object of the class \code{\link{data.frame}} containing the variables used in the analysis.
#' @param vcov the variance/covariance used in the estimation when using \code{\link[fixest]{feols}}. See \code{\link[fixest]{vcov.fixest}} for more details. Defaults to "iid".
#' @rdname sensemakr
#' @export
sensemakr.formula <- function(formula,
                              method = c("lm", "feols"),
                              vcov = "iid",
                              data,
                              treatment,
                              benchmark_covariates = NULL,
                              kd = 1,
                              ky = kd,
                              q = 1,
                              alpha = 0.05,
                              r2dz.x = NULL,
                              r2yz.dx = r2dz.x,
                              bound_label = "",
                              reduce = TRUE,
                              ...){
  check_formula(treatment = treatment,
                formula = formula,
                data = data)

  check_multipliers(ky = ky,
                    kd = kd)

  type <- match.arg(method, method)

  if(type == "lm") {
    reg.call <- call(type, formula = substitute(formula), data = substitute(data))
    outcome_model <- eval(reg.call)
  } else if(type == "feols") {
    if (!requireNamespace("fixest")) {
      stop("Please install the fixest package.")
    }
    outcome_model <- fixest::feols(fml = formula, data = data, vcov = vcov)
  }

  sensemakr(model = outcome_model,
            treatment = treatment,
            benchmark_covariates = benchmark_covariates,
            kd = kd,
            ky = ky,
            q = q,
            alpha = alpha,
            r2dz.x = r2dz.x,
            r2yz.dx = r2yz.dx,
            bound_label = bound_label,
            reduce = reduce,
            ...)
}

#' @inheritParams adjusted_estimate
#' @inheritParams ovb_partial_r2_bound
#' @export
#' @rdname sensemakr
sensemakr.numeric <- function(estimate,
                              se,
                              dof,
                              treatment = "D",
                              q = 1,
                              alpha = 0.05,
                              r2dz.x = NULL,
                              r2yz.dx = r2dz.x,
                              bound_label = "manual_bound",
                              r2dxj.x = NULL,
                              r2yxj.dx = r2dxj.x,
                              benchmark_covariates = "manual_benchmark",
                              kd = 1,
                              ky = kd,
                              reduce = TRUE,
                              ...){
  out <- list()
  out$info <- list(formula = "Data provided manually",
                   treatment = treatment,
                   q = q,
                   alpha = alpha,
                   reduce = reduce)

  # senstivity statistics
  out$sensitivity_stats <- sensitivity_stats.numeric(estimate = estimate,
                                             se = se,
                                             dof = dof,
                                             treatment = treatment,
                                             q = q,
                                             alpha = alpha)
  # bounds on ovb
  if (!is.null(r2dz.x)) {
    check_r2(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
    out$bounds <-  data.frame(r2dz.x = r2dz.x,
                              r2yz.dx = r2yz.dx,
                              bound_label = bound_label,
                              stringsAsFactors = FALSE)
    # compute adjusted effects
    out$bounds$adjusted_estimate = adjusted_estimate.numeric(estimate = estimate,
                                                     se = se,
                                                     dof = dof,
                                                     r2yz.dx = r2yz.dx,
                                                     r2dz.x = r2dz.x,
                                                     reduce = reduce)

    out$bounds$adjusted_se = adjusted_se.numeric(estimate = estimate,
                                         se = se,
                                         dof = dof,
                                         r2yz.dx = r2yz.dx,
                                         r2dz.x = r2dz.x)

    out$bounds$adjusted_t = adjusted_t.numeric(estimate = estimate,
                                       se = se,
                                       dof = dof,
                                       r2yz.dx = r2yz.dx,
                                       r2dz.x = r2dz.x,
                                       reduce = reduce)

    se_multiple <- qt(alpha/2, df = dof, lower.tail = F)
    out$bounds$adjusted_lower_CI <- out$bounds$adjusted_estimate - se_multiple*out$bounds$adjusted_se
    out$bounds$adjusted_upper_CI <- out$bounds$adjusted_estimate + se_multiple*out$bounds$adjusted_se
  } else{
    out$bounds <-  NULL
  }

  if (!is.null(r2dxj.x)) {

    bound_label <- label_maker(benchmark_covariate = benchmark_covariates,
                               kd = kd, ky = ky)

    bench_bounds <- ovb_partial_r2_bound.numeric(r2dxj.x = r2dxj.x,
                                         r2yxj.dx = r2yxj.dx,
                                         kd = kd,
                                         ky = ky,
                                         bound_label = bound_label)
    # compute adjusted effects
    bench_bounds$adjusted_estimate = adjusted_estimate.numeric(estimate = estimate,
                                                     se = se,
                                                     dof = dof,
                                                     r2yz.dx = bench_bounds$r2yz.dx,
                                                     r2dz.x =  bench_bounds$r2dz.x,
                                                     reduce = reduce)

    bench_bounds$adjusted_se = adjusted_se.numeric(estimate = estimate,
                                         se = se,
                                         dof = dof,
                                         r2yz.dx = bench_bounds$r2yz.dx,
                                         r2dz.x = bench_bounds$r2dz.x)

    bench_bounds$adjusted_t = adjusted_t.numeric(estimate = estimate,
                                         se = se,
                                         dof = dof,
                                         r2yz.dx = bench_bounds$r2yz.dx,
                                         r2dz.x =  bench_bounds$r2dz.x,
                                         reduce = reduce)

    se_multiple <- qt(alpha/2, df = dof, lower.tail = F)
    bench_bounds$adjusted_lower_CI <- bench_bounds$adjusted_estimate - se_multiple*bench_bounds$adjusted_se
    bench_bounds$adjusted_upper_CI <- bench_bounds$adjusted_estimate + se_multiple*bench_bounds$adjusted_se

    out$bounds <- rbind(out$bounds, bench_bounds)
  }

  class(out) <- "sensemakr"

  return(out)
}
