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
