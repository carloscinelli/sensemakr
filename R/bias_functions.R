# Bias functions ----------------------------------------------------------


# adjusted estimate -------------------------------------------------------

#' Bias-adjusted estimates, standard-errors and t-values
#'
#' This function...
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model with the
#' outcome regression or a numeric vector with the coefficient estimate.
#' @export
adjusted_estimate <- function(...){
  UseMethod("adjusted_estimate")
}

#' @param model an \code{lm} object with the outcome regression.
#' @param treatment a character vector with the name of the treatment variable of the model.
#' @rdname adjusted_estimate
#' @export
adjusted_estimate.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_estimate(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
}

#' @param estimate coefficient estimate.
#' @param se standard error of the coefficient estimate.
#' @param dof residual degrees of freedom of the regression.
#' @param r2dz.x hypothetical partial R2 of unobserved confounder Z with treatment D, given covariates X.
#' @param r2yz.dx hypothetical partial R2 of unobserved confounder Z with outcome Y, given covariates X and treatment D.
#' @param reduce should the bias adjustment reduce or increase the absolute value of the estimated coefficient? Default is \code{TRUE}.
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

  new_estimate
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

  # Run formual for SE of R^2
  sqrt((1 - r2yz.dx) / (1 - r2dz.x)) * se * sqrt(dof / (dof - 1))
}


#' @rdname adjusted_estimate
#' @export
adjusted_se.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_se(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
}


# adjusted t --------------------------------------------------------------

#' @rdname adjusted_estimate
#' @export
adjusted_t <- function(...){
  UseMethod("adjusted_t")
}


#' @rdname adjusted_estimate
#' @export
adjusted_t.numeric = function(estimate, se, dof, r2dz.x, r2yz.dx, reduce = TRUE, ...) {
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }
  new_estimate <- adjusted_estimate(estimate = estimate, r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof,reduce =  reduce)
  new_estimate / adjusted_se(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof)
}


#' @rdname adjusted_estimate
#' @export
adjusted_t.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_t(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
}


# bias --------------------------------------------------------------------


#' @rdname adjusted_estimate
#' @export
bias <- function(...){
  UseMethod("bias")
}

#' @rdname adjusted_estimate
#' @export
bias.numeric = function(se, dof, r2dz.x, r2yz.dx,  ...) {
  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x,se =  se, dof =  dof)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  sqrt(r2yz.dx * r2dz.x / (1 - r2dz.x)) * se * sqrt(dof)
}



#' @rdname adjusted_estimate
#' @export
bias.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, bias(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
}
