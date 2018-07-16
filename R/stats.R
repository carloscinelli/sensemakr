
# Basic stats -------------------------------------------------------------


# extract statistics from models ------------------------------------------

# helpers for all others
model_helper = function(model, covariate = NULL) {
  UseMethod("model_helper", model)
}

model_helper.default = function(model, covariate = NULL) {
  stop("The `partial_r2` function must be passed an `lm` model object. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

model_helper.lm = function(model, covariate = NULL) {
  # Quickly extract things from an lm object

  # If we have a dropped coefficient (multicolinearity), we're not going to
  # get an R^2 for this coefficient.
  warn_na_coefficients(model, covariate = covariate)

  # Let's avoid the NaN problem from dividing by zero
  error_if_no_dof(model)

  coefs <- coef(summary(model))
  covariate <- check_covariates(rownames(coefs), covariate)

  if (!is.null(covariate)) coefs <- coefs[covariate, ,drop = FALSE]

  list(
    estimates = coefs[, "Estimate"],
    se = coefs[, "Std. Error"],
    t_statistics = coefs[, "t value"],
    dof = model$df.residual
  )
}


warn_na_coefficients = function(model, covariate = NULL) {

  coefs <- coef(model)
  covariate <- check_covariates(names(coefs), covariate)

  if (!is.null(covariate))  coefs <- coefs[covariate]

  if (any(is.na(coefs))) {
    na_coefficients = names(coefs)[which(is.na(coefs))]
    coefficient_string = paste(na_coefficients, ", ")
    coefficient_string_plural = ifelse(length(na_coefficients) > 1,
                                       "coefficients",
                                       "coefficient")
    warning("Model contains 'NA' ", coefficient_string_plural, ". No R^2 can ",
            "be calculated for ", coefficient_string)
  }
}

error_if_no_dof = function(model) {
  if (model$df.residual == 0) {
    stop("The partial R^2 cannot be computed because there are 0 residual ",
         "degrees of freedom in the regression model provided.")
  }
}

check_covariates <- function(all_names, covariate){
  if (!is.null(covariate)) {
    if (!is.character(covariate)) stop("Covariate name must be a string.")
    if (!all(covariate %in% all_names)) stop("Covariates must match names in model.")
  }
  covariate
}

# partial r2 --------------------------------------------------------------


#' The RV (robusntess value), partial R^2, and partial f^2
#'
#' Statistics for OVB.
#'
#' @rdname stats
#' @export
partial_r2 = function(...) {
  UseMethod("partial_r2")
}

#' @rdname stats
#' @export
partial_r2.numeric <- function(t_statistic, dof){
  t_statistic^2 / (t_statistic^2 + dof)
}

#' @rdname stats
#' @export
partial_r2.lm = function(model, covariate = NULL) {

  # extract model data
  model_data <- model_helper(model, covariate = covariate)
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


# partial f2 --------------------------------------------------------------

#'
#'
#' @rdname stats
#' @export
partial_f2 = function(...) {
  UseMethod("partial_f2")
}

#' @rdname stats
#' @export
partial_f2.numeric <- function(t_statistic, dof){
  t_statistic^2 / dof
}

#' @rdname stats
#' @export
partial_f2.lm = function(model, covariate = NULL) {
  # extract model data
  model_data <- model_helper(model, covariate = covariate)
  t_statistic = model_data$t_statistics
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_f2(t_statistic = t_statistic, dof = dof)
}

partial_r2.default = function(model) {
  stop("The `partial_f2` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}


#' @export
partial_f = function(...) sqrt(partial_f2(...))


# robustness value --------------------------------------------------------


#' @rdname stats
#' @export
robustness_value = function(..., q = 1, alpha = NULL) {

  # check arguments
  check_q(q)
  check_alpha(alpha)

  UseMethod("robustness_value")
}

#' @rdname stats
#' @export
robustness_value.numeric <- function(t_statistic, dof, q =1, alpha = NULL){

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
  # attributes(rv) <- list(q = q, alpha = alpha)
  rv
}


#' Calculates the RV (robustness value)
#'
#'
#' @param model An `lm` object which will be used to produce a robustness value
#' @param t_statistic In lieu of supplying `model` and `covariate`, supply the
#' t-statistic of the treatment effect and `dof`
#' @param dof In lieu of supplying `model` and `covariate`, supply the residual
#' degrees of freedom of a model and `t_statistic`.
#' @param covariate A quoted character string describing the name of the
#' treatment covariate in the model object.
#' @param q A numeric fraction between 0 and 1 describing q% attenuation of the
#' observed treatment effect, defaults to 1 (complete attenuation)
#' @param alpha If specified, denotes a (1 - alpha) confidence interval used to
#' calculate the RV. If specified, the RV will reflect the percentage of
#' residual variation in the treatment/outcome that must be explained in order
#' for a (1 - alpha) confidence interval about the effect to cross the critical
#' threshold implied by `q`. If left empty or NULL, the point estimate must
#' cross the critical threshold.
#' @param ... Additional parameters, not currently used.
#'
#' @examples
#' @return A q-robustness value, which is a number from 0 to 1.
#' @importFrom stats qt
#' @rdname stats
#' @export
robustness_value.lm = function(model, covariate = NULL,
  q = 1, alpha = NULL) {

  # extract model data
  model_data <- model_helper(model, covariate = covariate)
  t_statistic = model_data$t_statistics
  dof         = model_data$dof

  # compute rv
  robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}


robustness_value.default = function(model) {
  stop("The `robustness_value` function must be passed either an `lm` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}


#' @rdname stats
#' @export
rv <- robustness_value

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







bias_in_r2 = function(r2y, r2d, se, dof) {
  # Error handling
  check_r2_parameters(r2y, r2d, se, dof)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  sqrt(r2y * r2d / (1 - r2d)) * se * sqrt(dof)
}

se_r2 = function(r2y, r2d, se, dof) {
  # Error handling
  check_r2_parameters(r2y, r2d, se, dof)

  # Run formual for SE of R^2
  sqrt((1 - r2y) / (1 - r2d)) * se * sqrt(dof / (dof - 1))
}

t_r2 = function(estimate, r2y, r2d, se, dof) {
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if(!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }

  (estimate - bias_in_r2(r2y, r2d, se, dof)) / se_r2(r2y, r2d, se, dof)
}

check_r2_parameters = function(r2y, r2d, se, dof) {
  # Invalid SE
  if(se < 0) {
    stop("Standard error provided must be a single non-negative number")
  }

  # Invalid DOF
  if(!is.numeric(dof) || length(dof) > 1 || dof <= 0) {
    stop("Degrees of freedom provided must be a single non-negative number.")
  }

  # Invalid R^2 Y / R^2 D.
  if(!is.numeric(r2y) || !is.numeric(r2d) ||
     any(r2y < 0) || any(r2y > 1) ||
     any(r2d < 0) || any(r2d > 1)) {
    stop("Provided R^2 of Y and D must both be numbers between 0 and 1.")
  }
}
