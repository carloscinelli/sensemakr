r_squared_helper = function(model) {
  UseMethod("r_squared_helper", model)
}

r_squared_helper.lm = function(model) {
  # If we have a dropped coefficient (multicolinearity), we're not going to
  # get an R^2 for this coefficient.
  warn_na_coefficients(model)

  # Extract coefficient t statistics
  t_statistic = coef(summary(model))[, "t value"]

  # Let's avoid the NaN problem from dividing by zero
  error_if_no_dof(model)

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  t_statistic^2 / (t_statistic^2 + model$df.residual)
}

r_squared_helper.default = function(model) {
  stop("The `r_squared_helper` function must be passed an `lm` model object. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

warn_na_coefficients = function(model) {
  if(any(is.na(coef(model)))) {
    na_coefficients = names(model$coefficients)[which(is.na(coef(model)))]
    coefficient_string = paste(na_coefficients, ", ")
    coefficient_string_plural = ifelse(length(na_coefficients) > 1,
                                       "coefficients",
                                       "coefficient")
    warning("Model contains 'NA' ", coefficient_string_plural, ". No R^2 can ",
            "be calculated for ", coefficient_string)
  }
}

error_if_no_dof = function(model) {
  if(model$df.residual == 0) {
    stop("The partial R^2 cannot be computed because there are 0 residual ",
         "degrees of freedom in the regression model provided.")
  }
}

