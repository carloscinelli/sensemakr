# Bias functions ----------------------------------------------------------



# bias --------------------------------------------------------------------


#' @rdname bias
#' @export
bias <- function(...){
  UseMethod("bias")
}

#' @rdname bias
#' @export
bias.numeric = function(se, dof, r2dz.x, r2yz.dx,  ...) {
  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x,se =  se, dof =  dof)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  sqrt(r2yz.dx * r2dz.x / (1 - r2dz.x)) * se * sqrt(dof)
}


#' @rdname bias
#' @export
bias.lm <- function(model, treatment,  r2dz.x, r2yz.dx){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, bias(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
}


# adjusted SE -------------------------------------------------------------

#' @rdname bias
#' @export
adjusted_se <- function(...){
  UseMethod("adjusted_se")
}

#' @rdname bias
#' @export
adjusted_se.numeric = function(se, dof, r2dz.x, r2yz.dx, ...) {
  # Error handling
  check_r2_parameters(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x,se =  se, dof =  dof)

  # Run formual for SE of R^2
  sqrt((1 - r2yz.dx) / (1 - r2dz.x)) * se * sqrt(dof / (dof - 1))
}

#' @rdname bias
#' @export
adjusted_se.lm <- function(model, treatment,  r2dz.x, r2yz.dx){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_se(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
}

# adjusted estimate -------------------------------------------------------

#' @rdname bias
#' @export
adjusted_estimate <- function(...){
  UseMethod("adjusted_estimate")
}

#' @rdname bias
#' @export
adjusted_estimate <- function(estimate, se, dof, r2dz.x, r2yz.dx,  reduce = TRUE, ...){
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
    new_estimate <- sign(estimate)*(abs(estimate) + bias(r2yz.dx, r2dz.x, se, dof))
  }

  new_estimate
}

#' @rdname bias
#' @export
adjusted_estimate.lm <- function(model, treatment,  r2dz.x, r2yz.dx){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_estimate(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
}

# adjusted t --------------------------------------------------------------

#' @rdname bias
#' @export
adjusted_t <- function(...){
  UseMethod("adjusted_t")
}

#' @rdname bias
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

#' @rdname bias
#' @export
adjusted_t.lm <- function(model, treatment,  r2dz.x, r2yz.dx){
  # extract model data
  model_data <- model_helper(model, covariates = treatment)
  with(model_data, adjusted_t(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
}

