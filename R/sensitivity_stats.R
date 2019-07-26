# General Sensitivity Statistics ------------------------------------------

# robustness value --------------------------------------------------------


#' Computes the robustness value
#'
#' @description
#' This function computes the robustness value of a regression coefficient.
#'
#' The robustness value describes the minimum strength of association (parameterized in terms of partial R2) that
#' omitted variables would need to have both with the treatment and with the outcome to change the estimated coefficient by
#' a certain amount (for instance, to bring it down to zero).
#'
#' For instance, a robustness value of 1\% means that an unobserved confounder that explain 1\% of the residual variance of the outcome
#' and 1\% of the residual variance of the treatment is strong enough to explain away the estimated effect. Whereas a robustness value of 90\%
#' means that any unobserved confounder that explain less than 90\% of the residual variance of both the outcome and the treatment assignment cannot
#' fully account for the observed effect. You may also compute robustness value taking into account sampling uncertainty. See details in Cinelli and Hazlett (2018).
#'
#' The function \link{robustness_value} can take as input an \code{\link{lm}} object or you may directly pass the t-value and degrees of freedom.
#'
#'
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model with the
#' regression model or a numeric vector with the t-value of the coefficient estimate
#'
#' @examples
#'
#' # using an lm object
#' ## loads data
#' data("darfur")
#'
#' ## fits model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' ## robustness value of directly harmed q =1 (reduce estimate to zero)
#' robustness_value(model, covariates = "directlyharmed")
#'
#' ## robustness value of directly harmed q = 1/2 (reduce estimate in half)
#' robustness_value(model, covariates = "directlyharmed", q = 1/2)
#'
#' ## robustness value of directly harmed q = 1/2, alpha = 0.05
#' ## (reduce estimate in half, with 95% confidence)
#' robustness_value(model, covariates = "directlyharmed", q = 1/2, alpha = 0.05)
#'
#' # you can also provide the statistics directly
#' robustness_value(t_statistic = 4.18445, dof = 783)


#' @return
#' The function returns a numerical vector with the robustness value. The arguments q and alpha are saved as attributes of the vector for reference.
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#' @export
robustness_value = function(...) {

  UseMethod("robustness_value")
}

#' @param model an \code{lm} object with the regression model.
#' @param covariates model covariates for which the robustness value will be computed. Default is to compute
#' the robustness value of all covariates.
#' @param q percent change of the effect estimate that would be deemed problematic.  Default is \code{1},
#' which means a reduction of 100\% of the current effect estimate (bring estimate to zero). It has to be greater than zero.
#' @param alpha significance level used for computation of the robustness value. If \code{NULL} (the default), the robustness value refers only to the point estimate, no sampling uncertainty is taken into account.
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
      qf = qf - (critical_f)
  }

  # Eqn. 19 from "Making Sense of Sensitivity"
  rv <- ifelse(qf < 0, 0,  0.5 * (sqrt(qf^4 + (4 * qf^2)) - qf^2))
  attributes(rv) <- list(names = names(rv), q = q, alpha = alpha, class = c("numeric","rv"))
  rv
}

#
# #' @export
# robustness_value.sensemakr <- function(x, ...){
#   x$sensitivity_stats[,c("rv_q","rv_qa")]
# }






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

#' Computes the partial R2 and partial (Cohen's) f2
#' @description
#'
#' These functions computes the partial R2 and the partial (Cohen's) f2 for a linear regression model. The partial R2
#' describes how much of the residual variance of the outcome (after partialing out the other covariates) a covariate explains.
#'
#' The partial R2 can be used as an extreme-scenario sensitivity analysis to omitted variables.
#' Considering an unobserved confounder that explains 100\% of the residual variance of the outcome,
#' the partial R2 describes how strongly associated with the treatment this unobserved confounder would need to be in order to explain away the estimated effect.
#' For details see Cinelli and Hazlett (2018).
#'
#' The partial (Cohen's) f2 is a common measure of effect size (a transformation of the partial R2) that can also be used directly
#' for sensitivity analysis using a bias factor table.
#'
#' The function \code{partial_r2} computes partial R2. The function \code{partial_f2} computes partial f2 and the function \code{partial_f} the partial f.
#' They can take as input an \code{\link{lm}} object or you may pass directly t-value and degrees of freedom.
#'
#' For partial R2 of groups of covariates, check \code{\link{group_partial_r2}}.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} object
#' with the regression model or a numeric vector with the t-value of the coefficient estimate
#'
#' @examples
#'
#' # using an lm object
#' ## loads data
#' data("darfur")
#'
#' ## fits model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' ## partial R2 of the treatment (directly harmed) with the outcome (peacefactor)
#' partial_r2(model, covariates = "directlyharmed")
#'
#' ## partial R2 of female with the outcome
#' partial_r2(model, covariates = "female")
#'
#' # you can also provide the statistics directly
#' partial_r2(t_statistic = 4.18445, dof = 783)
#'
#' @return
#' A numeric vector with the computed partial R2, f2, or f.
#'
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#' @export
partial_r2 = function(...) {
  UseMethod("partial_r2")
}


#' @param model an \code{lm} object with the regression model
#' @param covariates model covariates for which the partial R2 will be computed. Default is to compute
#' the partial R2 of all covariates.
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



# group_partial_r2 --------------------------------------------------------


#' Partial R2 of groups of covariates in a linear regression model
#'
#' This function computes the partial R2 of a group of covariates in a linear regression model.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} object
#' with the regression model or a numeric vector with the F-statistics for the group of covariates.
#'
#' @examples
#'
#' data("darfur")
#'
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' group_partial_r2(model, covariates = c("female", "pastvoted"))
#'
#' @return
#' A numeric vector with the computed partial R2.
#'
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


#' @param F.stats F-statistics for the group of covariates.
#' @param p number of parameters in the model.
#' @param dof residual degrees of freedom of the model.
#' @rdname group_partial_r2
#' @export
group_partial_r2.numeric <- function(F.stats, p, dof, ...){
  r2 <- F.stats*p / (F.stats*p + dof)
  r2 <- as.numeric(r2)
  r2
}




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
#'
#' @examples
#'
#' ## loads data
#' data("darfur")
#'
#' ## fits model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#' ## sensitivity stats for directly harmed
#' sensitivity_stats(model, treatment = "directlyharmed")
#'
#' ## you can  also pass the numeric values directly
#' sensitivity_stats(estimate = 0.09731582, se = 0.02325654, dof = 783)
#'
#' @return
#' A \code{data.frame} with the sensitivity statistics.
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
  if (se < 0 ) stop("Standard Error must be positive")
  if (!is.numeric(estimate)) stop("Estimate must be a numeric value")
  if (!is.numeric(se)) stop("Standard Error must be a numeric value")
  if (dof < 0) stop("Degrees of Freedom must be poisitive")
  t_statistic <- estimate/se
  sensitivity_stats <- data.frame(treatment = treatment, stringsAsFactors = FALSE)
  sensitivity_stats[["estimate"]] <- estimate
  sensitivity_stats[["se"]] <- se
  sensitivity_stats[["t_statistic"]] <- t_statistic
  sensitivity_stats[["r2yd.x"]] <- as.numeric(partial_r2(t_statistic = t_statistic, dof = dof))
  sensitivity_stats[["rv_q"]] <- (robustness_value(t_statistic = t_statistic, dof = dof, q = q))
  sensitivity_stats[["rv_qa"]] <- (robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha))
  sensitivity_stats[["f2yd.x"]] <- as.numeric(partial_f2(t_statistic = t_statistic, dof = dof))
  sensitivity_stats[["dof"]] <- dof
  sensitivity_stats
}

#
# sensitivity_stats.sensemakr <- function(x, ...){
#   x$sensitivity_stats
# }

# sanity checkers ---------------------------------------------------------


check_q <- function(q) {
  # Error: q non-numeric or out of bounds
  if (!is.numeric(q) || length(q) > 1 || q < 0) {
    stop("The `q` parameter must be a single number greater than 0.")
  }
}


check_alpha <- function(alpha) {
  # Error: alpha, if provided, was non-numeric or out of bounds
  if (!is.null(alpha) && (!is.numeric(alpha) || length(alpha) > 1 ||
                          alpha < 0 || alpha > 1)) {
    stop("The `alpha` parameter, if provided, must be a single number ",
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
