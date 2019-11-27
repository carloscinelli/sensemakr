# Bias functions ----------------------------------------------------------

# adjusted estimate -------------------------------------------------------

#' Bias-adjusted estimates, standard-errors and t-values
#'
#' @description
#'  These functions compute bias adjusted estimates (\code{adjusted_estimate}),
#'  standard-errors (\code{adjusted_se}) and t-values (\code{adjusted_t}),
#'  given a hypothetical strength of the confounder in the partial R2 parameterization.
#'
#' The functions work either with an \code{\link{lm}} object, or directly
#' passing in numerical inputs, such as the
#' current coefficient estimate, standard error and degrees of freedom.
#'
#' @param ... Arguments passed to other methods. First argument should either be an
#' \code{lm} model with the outcome regression or a numeric vector with the
#' coefficient estimate.
#'
#' @return
#' Numeric vector with bias, adjusted estimate, standard error, or t-value.
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B. (2020).
#' @examples
#' # loads data
#' data("darfur")
#'
#' # fits model
#' model <- lm(peacefactor ~ directlyharmed + age +
#'                           farmer_dar + herder_dar +
#'                            pastvoted + hhsize_darfur +
#'                            female + village, data = darfur)
#'
#' # computes adjusted estimate for confounder with  r2dz.x = 0.05, r2yz.dx = 0.05
#' adjusted_estimate(model, treatment = "directlyharmed", r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' # computes adjusted SE for confounder with  r2dz.x = 0.05, r2yz.dx = 0.05
#' adjusted_se(model, treatment = "directlyharmed", r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' # computes adjusted t-value for confounder with  r2dz.x = 0.05, r2yz.dx = 0.05
#' adjusted_t(model, treatment = "directlyharmed", r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' # Alternatively, pass in numerical values directly.
#' adjusted_estimate(estimate = 0.09731582, se = 0.02325654,
#'                   dof = 783, r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' adjusted_se(estimate = 0.09731582, se = 0.02325654,
#'             dof = 783, r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' adjusted_t(estimate = 0.09731582, se = 0.02325654,
#'            dof = 783, r2dz.x = 0.05, r2yz.dx = 0.05)
#'
#' @export
adjusted_estimate <- function(...){
  UseMethod("adjusted_estimate")
}

#' @param model An \code{lm} object with the outcome regression.
#' @param treatment A character vector with the name of the treatment variable
#' of the model.
#' @rdname adjusted_estimate
#' @export
adjusted_estimate.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  adj_estimate <- with(model_data,
       adjusted_estimate(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
  names(adj_estimate) <- treatment
  return(adj_estimate)
}

#' @param estimate Coefficient estimate.
#' @param se Standard error of the coefficient estimate.
#' @param dof Residual degrees of freedom of the regression.
#' @param r2dz.x Hypothetical partial R2 of unobserved confounder
#' Z with treatment D, given covariates X.
#' @param r2yz.dx Hypothetical partial R2 of unobserved confounder Z
#' with outcome Y, given covariates X and treatment D.
#' @param reduce Should the bias adjustment reduce or increase the
#' absolute value of the estimated coefficient? Default is \code{TRUE}.
#' @rdname adjusted_estimate
#' @export
adjusted_estimate.numeric <- function(estimate,
                                      se,
                                      dof,
                                      r2dz.x,
                                      r2yz.dx,
                                      reduce = TRUE, ...){
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }

  if (!is.logical(reduce) || length(reduce) > 1) {
    stop("Argument 'reduce' must be a single logical constant.")
  }

  if (reduce) {
    new_estimate <- sign(estimate)*(abs(estimate) - bias(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof))
  }

  if (!reduce) {
    new_estimate <- sign(estimate)*(abs(estimate) + bias(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof))
  }
  new_estimate <- unname(new_estimate)
  return(new_estimate)
}


# adjusted SE -------------------------------------------------------------


#' @rdname adjusted_estimate
#' @export
adjusted_se <- function(...){
  UseMethod("adjusted_se")
}

#' @rdname adjusted_estimate
#' @export
adjusted_se.numeric = function(se, dof, r2dz.x, r2yz.dx, ...) {
  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x,se =  se, dof =  dof)

  # Run formula for SE of R^2
  new_se <- sqrt((1 - r2yz.dx) / (1 - r2dz.x)) * se * sqrt(dof / (dof - 1))
  new_se <- unname(new_se)
  return(new_se)
}


#' @rdname adjusted_estimate
#' @export
adjusted_se.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  new_se <- with(model_data, adjusted_se(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(new_se) <- treatment
  return(new_se)
}


# adjusted t --------------------------------------------------------------

#' @rdname adjusted_estimate
#' @export
adjusted_t <- function(...){
  UseMethod("adjusted_t")
}


#' @rdname adjusted_estimate
#' @export
adjusted_t.numeric = function(estimate, se, dof, r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...) {
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }
  new_estimate <- adjusted_estimate(estimate = estimate, r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof,reduce =  reduce)
 new_t <-  (new_estimate - h0) / adjusted_se(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof)
 unname(new_t)
 return(new_t)
}


#' @rdname adjusted_estimate
#' @param h0 Null hypothesis for computation of the t-value. Default is zero.
#' @export
adjusted_t.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  new_t <- with(model_data, adjusted_t(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
                                       r2yz.dx = r2yz.dx, reduce = reduce, h0 = h0))
  names(new_t) <- treatment
  return(new_t)
}


# adjusted_partial_r2 -----------------------------------------------------


#' @rdname adjusted_estimate
#' @export
adjusted_partial_r2 <- function(...){
  UseMethod("adjusted_partial_r2")
}

#' @rdname adjusted_estimate
#' @export
adjusted_partial_r2.numeric <- function(estimate, se, dof, r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  new_t <- adjusted_t(estimate = estimate, r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof, reduce =  reduce, h0 = h0)
  partial_r2(t_statistic = new_t, dof = dof - 1)
}

#' @rdname adjusted_estimate
#' @export
adjusted_partial_r2.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_partial_r2(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
                              r2yz.dx = r2yz.dx, reduce = reduce, h0 = h0))
}


# bias --------------------------------------------------------------------


#' @rdname adjusted_estimate
#' @export
bias <- function(...){
  UseMethod("bias")
}

#' @rdname adjusted_estimate
#' @export
bias.numeric <- function(se, dof, r2dz.x, r2yz.dx,  ...) {
  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x,se =  se, dof =  dof)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  bias <- bias_factor(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx) * se * sqrt(dof)
  bias <- unname(bias)
  return(bias)
}



#' @rdname adjusted_estimate
#' @export
bias.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  bias <- with(model_data, bias(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(bias) <- treatment
  return(bias)
}


#' @rdname adjusted_estimate
#' @export
relative_bias <- function(...){
  UseMethod("relative_bias")
}


#' @rdname adjusted_estimate
#' @export
relative_bias.lm <- function(model, treatment, r2dz.x, r2yz.dx, ...){
  model_data <- model_helper(model, covariates = treatment)
  rel.bias   <- with(model_data, relative_bias(estimate = estimate,
                                               se = se, dof = dof,
                                               r2dz.x = r2dz.x,
                                               r2yz.dx = r2yz.dx))
  names(rel.bias) <- treatment
  return(rel.bias)
}

#' @rdname adjusted_estimate
#' @export
relative_bias.numeric <- function(estimate, se, dof, r2dz.x, r2yz.dx,  ...) {

  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x, se =  se, dof =  dof)
  t_statistic <- abs(estimate/se)
  f <- partial_f(t_statistic = t_statistic, dof = dof)
  BF <- bias_factor(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  q <- BF/f
  return(q)
}

#' @rdname adjusted_estimate
#' @export
bias_factor <- function(r2dz.x, r2yz.dx){
  BF <- sqrt(r2yz.dx * r2dz.x / (1 - r2dz.x))
  return(BF)
}

#' @rdname adjusted_estimate
#' @param r.est restricted estimate. A numerical vector.
#' @param est unrestricted estimate. A numerical vector.
#'@export
rel_bias <- function(r.est, est){
  (r.est - est)/r.est
}
