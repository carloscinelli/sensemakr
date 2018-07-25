# General Sensitivity Statistics ------------------------------------------

# robustness value --------------------------------------------------------


#' Robustness value of a regression coefficient
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model with the
#' regression model or a numeric vector with the t-value of the coefficient estimate
#' @export
robustness_value = function(...) {

  UseMethod("robustness_value")
}

#' @param model an \code{lm} object with the regression model.
#' @param covariates model covariates for which the robustness value will be computed. Default is to compute
#' the robustness value of all covariates.
#' @param q  hypothesized proportion of bias relative to original estimate due to unobserved confounders,
#' used for computation of the robustness value. It has to be greater than zero. Default is \code{1}.
#' @param alpha significance level used for computation of the robustness value. Default is \code{0.05}.
#' @rdname robustness_value
#' @export
#' @importFrom stats setNames
robustness_value.lm = function(model,
                               covariates = NULL,
                               q = 1,
                               alpha = NULL, ...) {

  # check arguments
  check_q(q)
  check_alpha(alpha)

  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # compute rv
  robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}


robustness_value.default = function(model, ...) {
  stop("The `robustness_value` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

#' @param t_statistic \code{numeric} vector with the t-value of the coefficient estimates
#' @param  dof residual degrees of freedom of the regression
#' @rdname robustness_value
#' @export
robustness_value.numeric <- function(t_statistic, dof, q =1, alpha = NULL, ...){

  # check arguments
  check_q(q)
  check_alpha(alpha)

  # Calculate RV
  # QF is q * absolute value(partial f). We can get partial f from t for cov.
  # of interest.
  qf = q * abs(t_statistic / sqrt(dof))

  # If we have an alpha value (i.e. we want to know whether the result will be
  # significant, rather than whether it will be >0) we need to adjust the qf
  # accordingly. [eqn. 18 from "Making Sense of Sensitivity"]
  if (!is.null(alpha)) {
    critical_f = abs(qt(alpha / 2,
                        df = dof - 1)) / sqrt(dof - 1)
    if (critical_f > qf) {
      qf = 0
    } else {
      qf = qf - (critical_f)
    }
  }

  # Eqn. 19 from "Making Sense of Sensitivity"
  rv <- 0.5 * (sqrt(qf^4 + (4 * qf^2)) - qf^2)
  attributes(rv) <- list(names = names(rv), q = q, alpha = alpha, class = c("numeric","rv"))
  rv
}








#' @export
print.rv <- function(x, ...){
  value <- x
  attributes(value) <- list(names = names(value))
  class(value) <- "numeric"
  print(value)
  q <- attr(x, "q")
  alpha <- attr(x, "alpha")
  cat("Parameters: q =", q)
  if (!is.null(alpha)) cat(", alpha =", alpha,"\n")
}



# partial r2 --------------------------------------------------------------

#' Partial R2 and partial (Cohen's) f2 of covariates in a linear regression model
#' @description
#' The \code{partial_r2} function computes partial R2.
#'
#' The \code{partial_f2} function computes partial f2 and \code{partial_f} the partial f.
#'
#' For partial R2 of groups of covariates, check \code{\link{group_partial_r2}}.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} object
#' with the regression model or a numeric vector with the t-value of the coefficient estimate
#' @export
partial_r2 = function(...) {
  UseMethod("partial_r2")
}


#' @param model an \code{lm} object with the regression model
#' @param covariates model covariates for which the partial R2 will be computed. Default is to compute
#' the partial R2 of all covariates
#' @rdname partial_r2
#' @export
partial_r2.lm = function(model, covariates = NULL, ...) {

  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_r2(t_statistic = t_statistic, dof = dof)
}

#' @inheritParams robustness_value
#' @rdname partial_r2
#' @export
partial_r2.numeric <- function(t_statistic, dof, ...){
  t_statistic^2 / (t_statistic^2 + dof)
}

partial_r2.default = function(model) {
  stop("The `partial_r2` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}




# group_partial_r2 --------------------------------------------------------


#' Partial R2 of groups of covariates in a linear regression model
#'
#'
#' @param ... test
#' @export
group_partial_r2 <- function(...){
  UseMethod("group_partial_r2")
}




#' @inheritParams partial_r2
#' @rdname group_partial_r2
#' @export
group_partial_r2.lm <- function(model, covariates, ...){
  if (missing(covariates)) stop("Argument covariates missing.")

  coefs <- coef(model)

  check_covariates(all_names = names(coefs), covariates = covariates)

  # coefficiens
  coefs <- coefs[covariates]

  # vcov matrix
  V <- vcov(model)[covariates, covariates, drop = FALSE]

  # degrees of freedom
  dof <- df.residual(model)


  # compute F and R2
  p <- length(coefs)
  f <- (t(coefs) %*% solve(V) %*% coefs)/p

  group_partial_r2(F.stats = f, p = p, dof = dof)

}


#' @param F.stats F statistics
#' @param p number of parameters in the model
#' @param dof residual degrees of freedom of the model
#' @rdname group_partial_r2
#' @export
group_partial_r2.numeric <- function(F.stats, p, dof, ...){
  r2 <- F.stats*p / (F.stats*p + dof)
  r2 <- as.numeric(r2)
  r2
}

# partial f2 --------------------------------------------------------------

#' @rdname partial_r2
#' @export
partial_f2 = function(...) {
  UseMethod("partial_f2")
}

#' @rdname partial_r2
#' @export
partial_f2.numeric <- function(t_statistic, dof, ...){
  t_statistic^2 / dof
}

#' @rdname partial_r2
#' @export
partial_f2.lm = function(model, covariates = NULL, ...) {
  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_f2(t_statistic = t_statistic, dof = dof)
}

partial_r2.default = function(model, ...) {
  stop("The `partial_f2` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}


#' @rdname partial_r2
#' @export
partial_f = function(...) sqrt(partial_f2(...))



# sensitivity stats -------------------------------------------------------

#' Sensitivity statistics for regression coefficients
#'
#'
#' @description
#' Convenience function that computes the \code{\link{robustness_value}},
#' \code{\link{partial_r2}} and \code{\link{partial_f2}} of the coefficient of interest. It
#' returns a \code{data.frame} with all quantities of interest.
#'
#' @inheritParams adjusted_estimate
#' @export
sensitivity_stats <- function(...){
  UseMethod("sensitivity_stats")
}

#' @inheritParams adjusted_estimate
#' @inheritParams robustness_value
#' @rdname sensitivity_stats
#' @export
sensitivity_stats.lm <- function(model,
                                 treatment,
                                 q = 1,
                                 alpha = 0.05, ...)
{

  model_data <- model_helper(model, covariates = treatment)
  sensitivity_stats <- with(model_data, sensitivity_stats(estimate = estimate,
                                                          se = se,
                                                          dof = dof,
                                                          treatment = treatment,
                                                          q = q,
                                                          alpha = alpha,
                                                          t_statistics = t_statistic))
  sensitivity_stats
}

#' @inheritParams adjusted_estimate
#' @rdname sensitivity_stats
#' @export
sensitivity_stats.numeric <- function(estimate,
                                      se,
                                      dof,
                                      treatment = "treatment",
                                      q = 1,
                                      alpha = 0.05,
                                      ...)
{
  t_statistic <- estimate/se
  sensitivity_stats <- data.frame(treatment = treatment, stringsAsFactors = FALSE)
  sensitivity_stats[["estimate"]] <- estimate
  sensitivity_stats[["se"]] <- se
  sensitivity_stats[["t_statistic"]] <- t_statistic
  sensitivity_stats[["r2yd.x"]] <- as.numeric(partial_r2(t_statistic = t_statistic, dof = dof))
  sensitivity_stats[["rv_q"]] <- as.numeric(robustness_value(t_statistic = t_statistic, dof = dof, q = q))
  sensitivity_stats[["rv_qa"]] <- as.numeric(robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha))
  sensitivity_stats[["f2yd.x"]] <- as.numeric(partial_f2(t_statistic = t_statistic, dof = dof))
  sensitivity_stats[["dof"]] <- dof
  sensitivity_stats
}




# sanity checkers ---------------------------------------------------------


check_q <- function(q) {
  # Error: q non-numeric or out of bounds
  if (!is.numeric(q) || length(q) > 1 || q < 0) {
    stop("The `q` parameter must be a numeric quantity greater than 0.")
  }
}


check_alpha <- function(alpha) {
  # Error: alpha, if provided, was non-numeric or out of bounds
  if (!is.null(alpha) && (!is.numeric(alpha) || length(alpha) > 1 ||
                          alpha < 0 || alpha > 1)) {
    stop("The `alpha` parameter, if provided, must be a numeric quantity ",
         "between 0 and 1.")
  }
}


check_r2_parameters = function(r2yz.dx, r2dz.x, se, dof) {
  # Invalid SE
  if (se < 0) {
    stop("Standard error provided must be a single non-negative number")
  }

  # Invalid DOF
  if (!is.numeric(dof) || length(dof) > 1 || dof <= 0) {
    stop("Degrees of freedom provided must be a single non-negative number.")
  }

  # Invalid R^2 Y / R^2 D.
  if (!is.numeric(r2yz.dx) || !is.numeric(r2dz.x) ||
     any(r2yz.dx < 0) || any(r2yz.dx > 1) ||
     any(r2dz.x < 0) || any(r2dz.x > 1)) {
    stop("Provided partial R^2 of Y and D must both be numbers between 0 and 1.")
  }
}




# model helpers -----------------------------------------------------------

# helpers for all others
model_helper = function(model, covariates = NULL) {
  UseMethod("model_helper", model)
}

model_helper.default = function(model, covariates = NULL) {
  stop("The `partial_r2` function must be passed an `lm` model object. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

model_helper.lm = function(model, covariates = NULL) {
  # Quickly extract things from an lm object

  # If we have a dropped coefficient (multicolinearity), we're not going to
  # get an R^2 for this coefficient.
  warn_na_coefficients(model, covariates = covariates)

  # Let's avoid the NaN problem from dividing by zero
  error_if_no_dof(model)

  coefs <- coef(summary(model))
  covariates <- check_covariates(rownames(coefs), covariates)

  if (!is.null(covariates)) coefs <- coefs[covariates, ,drop = FALSE]

  list(
    covariates = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se = coefs[, "Std. Error"],
    t_statistics = coefs[, "t value"],
    dof = model$df.residual
  )
}


warn_na_coefficients = function(model, covariates = NULL) {

  coefs <- coef(model)
  covariates <- check_covariates(names(coefs), covariates)

  if (!is.null(covariates))  coefs <- coefs[covariates]

  if (any(is.na(coefs))) {
    na_coefficients = names(coefs)[which(is.na(coefs))]
    coefficient_string = paste(na_coefficients, collapse = ", ")
    coefficient_string_plural = ifelse(length(na_coefficients) > 1,
                                       "coefficients",
                                       "coefficient")
    warning("Model contains 'NA' ", coefficient_string_plural, ". No partial R^2 can ",
            "be calculated for: ", coefficient_string)
  }
}

error_if_no_dof = function(model) {
  if (model$df.residual == 0) {
    stop("There are 0 residual ",
         "degrees of freedom in the regression model provided.")
  }
}

check_covariates <- function(all_names, covariates){
  if (!is.null(covariates)) {
    if (!is.character(covariates)) stop("Treatment and covariates names must be a string.")
    if (!all(covariates %in% all_names)) {
      idx <- which(!covariates %in% all_names)
      not_found <- paste(covariates[idx], collapse = ", ")
      stop("Variables not found in model: ", not_found)
    }
  }
  covariates
}
