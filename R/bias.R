# Bias functions ----------------------------------------------------------


#' @rdname bias
#' @export
bias = function(r2yz.dx, r2dz.x, se, dof) {
  # Error handling
  check_r2_parameters(r2yz.dx, r2dz.x, se, dof)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  sqrt(r2yz.dx * r2dz.x / (1 - r2dz.x)) * se * sqrt(dof)
}

#' @rdname bias
#' @export
adjusted_se = function(r2yz.dx, r2dz.x, se, dof) {
  # Error handling
  check_r2_parameters(r2yz.dx, r2dz.x, se, dof)

  # Run formual for SE of R^2
  sqrt((1 - r2yz.dx) / (1 - r2dz.x)) * se * sqrt(dof / (dof - 1))
}

#' @rdname bias
#' @export
adjusted_estimate <- function(estimate, r2yz.dx, r2dz.x, se, dof, reduce = TRUE){
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }

  if (!is.logical(reduce) || length(reduce) > 1) {
    stop("Argument 'reduce' must be a single logical constant.")
  }

  if (reduce) {
    new_estimate <- sign(estimate)*(abs(estimate) - bias(r2yz.dx, r2dz.x, se, dof))
  }

  if (!reduce) {
    new_estimate <- sign(estimate)*(abs(estimate) + bias(r2yz.dx, r2dz.x, se, dof))
  }

  new_estimate
}

#' @rdname bias
#' @export
adjusted_t = function(estimate, r2yz.dx, r2dz.x, se, dof, reduce = TRUE) {
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }
  new_estimate <- adjusted_estimate(estimate, r2yz.dx, r2dz.x, se, dof, reduce)
  new_estimate / adjusted_se(r2yz.dx, r2dz.x, se, dof)
}
