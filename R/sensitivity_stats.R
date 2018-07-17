# General Sensitivity Statistics ------------------------------------------

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

# partial r2 --------------------------------------------------------------

#' test
#' @param ... test
#' @export
partial_r2 = function(...) {
  UseMethod("partial_r2")
}

#' @export
partial_r2.numeric <- function(t_statistic, dof, ...){
  t_statistic^2 / (t_statistic^2 + dof)
}

#' @export
partial_r2.lm = function(model, covariates = NULL, ...) {

  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = model_data$t_statistics
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_r2(t_statistic = t_statistic, dof = dof)
}


partial_r2.default = function(model) {
  stop("The `partial_r2` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}


# group_partial_r2 --------------------------------------------------------


#' test
#' @param ... test
#' @export
group_partial_r2 <- function(...){
  UseMethod("group_partial_r2")
}


#' @export
group_partial_r2.numeric <- function(f_statistic, p, dof, ...){
  r2 <- f_statistic*p / (f_statistic*p + dof)
  r2 <- as.numeric(r2)
  r2
}


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

  group_partial_r2(f_statistic = f, p = p, dof = dof)

}



# partial f2 --------------------------------------------------------------

#' test
#' @param ... test
#' @export
partial_f2 = function(...) {
  UseMethod("partial_f2")
}


#' @export
partial_f2.numeric <- function(t_statistic, dof, ...){
  t_statistic^2 / dof
}


#' @export
partial_f2.lm = function(model, covariates = NULL, ...) {
  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = model_data$t_statistics
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


#' test
#' @param ... test
#' @export
partial_f = function(...) sqrt(partial_f2(...))


# robustness value --------------------------------------------------------


#' test
#' @param ... test
#' @param q test
#' @param alpha test
#' @export
robustness_value = function(..., q = 1, alpha = NULL) {

  # check arguments
  check_q(q)
  check_alpha(alpha)

  UseMethod("robustness_value")
}


#' @export
robustness_value.numeric <- function(t_statistic, dof, q =1, alpha = NULL, ...){

  # Calculate RV
  # QF is q * absolute value(partial f). We can get partial f from t for cov.
  # of interest.
  qf = q * abs(t_statistic / sqrt(dof))

  # If we have an alpha value (i.e. we want to know whether the result will be
  # significant, rather than whether it will be >0) we need to adjust the qf
  # accordingly. [eqn. 18 from "Making Sense of Sensitivity"]
  if (!is.null(alpha)) {
    critical_t = abs(qt(alpha / 2,
                        df = dof - 1))
    qf = qf - (critical_t / sqrt(dof - 1))
  }

  # Eqn. 19 from "Making Sense of Sensitivity"
  rv <- 0.5 * (sqrt(qf^4 + (4 * qf^2)) - qf^2)
  attributes(rv) <- list(q = q, alpha = alpha, class = c("numeric","rv"))
  rv
}




#' @export
robustness_value.lm = function(model,
                               covariates = NULL,
                               q = 1,
                               alpha = NULL, ...) {

  # extract model data
  model_data <- model_helper(model, covariates = covariates)
  t_statistic = model_data$t_statistics
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



#' @export
print.rv <- function(x, ...){
  value <- as.numeric(x)
  print(value)
  q <- attr(x, "q")
  alpha <- attr(x, "alpha")
  cat("Parameters: q =", q)
  if (!is.null(alpha)) cat(", alpha =", alpha,"\n")
}


# sensitivity stats -------------------------------------------------------

#' test
#' @param ... test
#' @export
sensitivity_stats <- function(...){
  UseMethod("sensitivity_stats")
}

#' @export
sensitivity_stats.numeric <- function(estimate,
                                      se,
                                      dof,
                                      treatment = "treatment",
                                      q = 1,
                                      alpha = 0.05,
                                      t_statistic = estimate/se, ...)
{
  sensitivity_stats <- data.frame(treatment = treatment, stringsAsFactors = FALSE)
  sensitivity_stats[["estimate"]] <- estimate
  sensitivity_stats[["se"]] <- se
  sensitivity_stats[["t_statistic"]] <- t_statistic
  sensitivity_stats[["r2yd.x"]] <- partial_r2(t_statistic = t_statistic, dof = dof)
  sensitivity_stats[["rv_q"]] <- robustness_value(t_statistic = t_statistic, dof = dof, q = q)
  sensitivity_stats[["rv_qa"]] <- robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)
  sensitivity_stats[["f2yd.x"]] <- partial_f2(t_statistic = t_statistic, dof = dof)
  sensitivity_stats[["dof"]] <- dof
  sensitivity_stats
}


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


# sanity checkers ---------------------------------------------------------


check_q <- function(q) {
  # Error: q non-numeric or out of bounds
  if (!is.numeric(q) || length(q) > 1 || q < 0 || q > 1) {
    stop("The `q` parameter must be a numeric quantity between 0 and 1.")
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




