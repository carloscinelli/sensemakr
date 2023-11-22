# Bias functions ----------------------------------------------------------

# adjusted critical value -------------------------------------------------

#' Bias-adjusted critical values
#'
#' @description
#' These functions compute bias adjusted critical values for a given postulated strength of omitted variable with the dependent and independent variables of an OLS regression.
#'
#' Researchers can thus easily perform sensitivity analysis by simply substituting traditional thresholds with bias-adjusted thresholds, when testing a particular null hypothesis, or when constructing confidence intervals.
#'
#'
#' @inheritParams adjusted_estimate
#' @param alpha significance level. Default is `0.05`.
#' @param max if `TRUE` (default) it computes the worst possible adjusted critical threshold for an omitted variable with strength limited by `r2dz.x` and `r2yz.dx`.
#'
#' @return Numeric vector with bias-adjusted critical values.
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#'
#'
#' Cinelli, C. and Hazlett, C. (2023), "An Omitted Variable Bias Framework for Sensitivity Analysis of Instrumental Variables."
#' @examples
#'
#' # traditional critical threshold (no confounding) is 1.96 (dof = 1e4)
#' adjusted_critical_value(r2dz.x = 0, r2yz.dx = 0, dof = 1e4, alpha = 0.05)
#'
#' # adjusted critical threshold, r2 = 1% is 2.96 (dof = 1e4)
#' adjusted_critical_value(r2dz.x = 0.01, r2yz.dx = 0.01, dof = 1e4, alpha = 0.05)
#'
#'
#' @export
adjusted_critical_value <- function(r2dz.x, r2yz.dx, dof, alpha = 0.05, max = T){

  # check arguments
  check_alpha(alpha)
  check_dof(dof)
  check_r2(r2dz.x, r2yz.dx)


  # traditional critical value
  t.crit <- sqrt(qf(p = 1 - (alpha), df1 = 1, df2 = dof-1, lower.tail = TRUE))
  f.crit <- t.crit/sqrt(dof)
  if (max == T) {
    cond <- r2yz.dx  < f.crit^2 * (r2dz.x/(1-r2dz.x))
    r2ys <- rep(NA, length(cond))
    r2ys[!cond] <- r2yz.dx[!cond]
    r2ys[cond] <- r2dz.x[cond]/(f.crit[cond]^2 + r2dz.x[cond])
    r2yz.dx <- r2ys
  }
  sef <- sef(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  bf  <- bf(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  ddof <- sqrt((dof/(dof-1)))
  t.dagger <- t.crit*sef*ddof + bf*sqrt(dof)
  return(t.dagger)
}

# #' @export t.dagger
# #' @rdname adjusted_critical_value
# t.dagger <- adjusted_critical_value

# adjusted estimate -------------------------------------------------------

#' Bias-adjusted estimates, standard-errors, t-values and confidence intervals
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
#' @param ... arguments passed to other methods.
#' @param model An \code{lm} or \code{fixest} object with the outcome regression.
#'
#' @return
#' Numeric vector with bias, adjusted estimate, standard error, or t-value.
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
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


#' @param treatment A character vector with the name of the treatment variable
#' of the model.
#' @rdname adjusted_estimate
#' @export
adjusted_estimate.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  adj_estimate <- with(model_data,
       adjusted_estimate.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
  names(adj_estimate) <- rep(treatment, length(adj_estimate))
  return(adj_estimate)
}

#' @rdname adjusted_estimate
#' @export
adjusted_estimate.fixest <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, ...){
  # extract model data
  model_data <- model_helper.fixest(model, covariates = treatment)
  adj_estimate <- with(model_data,
                       adjusted_estimate.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx, reduce = reduce))
  names(adj_estimate) <- rep(treatment, length(adj_estimate))
  return(adj_estimate)
}

#' @param estimate Coefficient estimate.
#' @param se standard error of the coefficient estimate.
#' @param dof residual degrees of freedom of the regression.
#' @param r2dz.x hypothetical partial R2 of unobserved confounder
#' Z with treatment D, given covariates X.
#' @param r2yz.dx hypothetical partial R2 of unobserved confounder Z
#' with outcome Y, given covariates X and treatment D.
#' @param reduce should the bias adjustment reduce or increase the
#' absolute value of the estimated coefficient? Default is \code{TRUE}.
#' @rdname adjusted_estimate
#' @export
adjusted_estimate.numeric <- function(estimate,
                                      se,
                                      dof,
                                      r2dz.x,
                                      r2yz.dx,
                                      reduce = TRUE,
                                      ...){

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
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)

  # Run formula for SE of R^2
  new_se <- sef(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx) * se * sqrt(dof / (dof - 1))
  new_se <- unname(new_se)
  return(new_se)
}


#' @rdname adjusted_estimate
#' @export
adjusted_se.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  new_se <- with(model_data, adjusted_se.numeric(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(new_se) <- rep(treatment, length(new_se))
  return(new_se)
}

#' @rdname adjusted_estimate
#' @export
adjusted_se.fixest <- function(model, treatment,  r2dz.x, r2yz.dx, message = TRUE, ...){
  # extract model data
  if(message){
    vcov_type <- model$call$vcov
    if(!is.null(vcov_type)){
      if(vcov_type!= "iid"){
        message("Note for fixest: using 'iid' standard errors. Support for robust standard errors coming soon.")
      }
    }
  }
  model_data <- model_helper.fixest(model, covariates = treatment)
  new_se <- with(model_data, adjusted_se.numeric(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(new_se) <- rep(treatment, length(new_se))
  return(new_se)
}


# adjusted ci -------------------------------------------------------------

#' @rdname adjusted_estimate
#' @export
adjusted_ci <- function(...){
  UseMethod("adjusted_ci")
}


#' @rdname adjusted_estimate
#' @param which which limit of the confidence interval to show? Options are "both", lower limit ("ll") or upper limit ("ul").
#' @param alpha significance level.
#' @export
adjusted_ci.lm <- function(model, treatment,  r2dz.x, r2yz.dx,
                           which = c("both", "ll", "ul"),
                           reduce = TRUE, alpha = 0.05, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  new_ci <- with(model_data, adjusted_ci.numeric(estimate = estimate, se = se, dof = dof,
                                                 r2dz.x = r2dz.x, r2yz.dx = r2yz.dx,
                                                 which = which,
                                                 reduce = reduce, alpha = alpha))
  names(new_ci) <- rep(treatment, length(new_ci))
  return(new_ci)
}

#' @rdname adjusted_estimate
#' @param message should messages be printed? Default = TRUE.
#' @export
adjusted_ci.fixest <- function(model, treatment,  r2dz.x, r2yz.dx,
                               which = c("both", "ll", "ul"),
                               reduce = TRUE, alpha = 0.05, message = T, ...){
  if(message){
    vcov_type <- model$call$vcov
    if(!is.null(vcov_type)){
      if(vcov_type!= "iid"){
        message("Note for fixest: using 'iid' standard errors. Support for robust standard errors coming soon.")
      }
    }
  }
  # extract model data
  model_data <- model_helper.fixest(model, covariates = treatment)
  new_ci <- with(model_data, adjusted_ci.numeric(estimate = estimate, se = se, dof = dof,
                                                 r2dz.x = r2dz.x, r2yz.dx = r2yz.dx,
                                                 which = which,
                                                 reduce = reduce, alpha = alpha))
  names(new_ci) <- rep(treatment, length(new_ci))
  return(new_ci)
}



#' @rdname adjusted_estimate
#' @export
adjusted_ci.numeric = function(estimate, se, dof, r2dz.x, r2yz.dx,
                               which = c("both", "ll", "ul"),
                               reduce = TRUE, alpha = 0.05, ...) {
  # Error handling
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)
  which <- match.arg(which)
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }
  t.crit <- sqrt(qf(p = 1 - (alpha), df1 = 1, df2 = dof - 1, lower.tail = TRUE))

  new_estimate <- adjusted_estimate.numeric(estimate = estimate,
                                            r2yz.dx = r2yz.dx, r2dz.x = r2dz.x,
                                            se = se, dof = dof,reduce =  reduce)
  new_se <- adjusted_se(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof)

  ll <- new_estimate - t.crit*new_se
  ul <- new_estimate + t.crit*new_se
  ci <- cbind(ll = ll, ul = ul)
  if (which == "both"){
    return(ci)
  } else {
  return(ci[,which])
  }
}



# adjusted t --------------------------------------------------------------

#' @rdname adjusted_estimate
#' @export
adjusted_t <- function(...){
  UseMethod("adjusted_t")
}

#' @rdname adjusted_estimate
#' @param h0 Null hypothesis for computation of the t-value. Default is zero.
#' @export
adjusted_t.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  new_t <- with(model_data, adjusted_t.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
                                       r2yz.dx = r2yz.dx, reduce = reduce, h0 = h0))
  names(new_t) <- rep(treatment, length(new_t))
  return(new_t)
}

#' @rdname adjusted_estimate
#' @param h0 Null hypothesis for computation of the t-value. Default is zero.
#' @param message should messages be printed? Default = TRUE.
#' @export
adjusted_t.fixest <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, message = T, ...){
  if(message){
    vcov_type <- model$call$vcov
    if(!is.null(vcov_type)){
      if(vcov_type!= "iid"){
        message("Note for fixest: using 'iid' standard errors. Support for robust standard errors coming soon.")
      }
    }
  }
  # extract model data
  model_data <- model_helper.fixest(model, covariates = treatment)
  new_t <- with(model_data, adjusted_t.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
                                       r2yz.dx = r2yz.dx, reduce = reduce, h0 = h0))
  names(new_t) <- rep(treatment, length(new_t))
  return(new_t)
}

#' @rdname adjusted_estimate
#' @export
adjusted_t.numeric = function(estimate, se, dof, r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...) {
  # Error handling
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)
  # Error handling (most handled through dispatch to bias/se, but need
  # to make sure estimate is also valid)
  if (!is.numeric(estimate) || length(estimate) > 1) {
    stop("Estimate provided must be a single number.")
  }
  new_estimate <- adjusted_estimate.numeric(estimate = estimate, r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof,reduce =  reduce)
  new_t <-  (new_estimate - h0) / adjusted_se(r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof)
  unname(new_t)
  attributes(new_t) <- list(h0 = h0)
  class(new_t) <- c("numeric", "t_stats") # this is on purpose, numeric
  return(new_t)
}

#' @export
print.t_stats <- function(x, ...){
  value <- x
  attributes(value) <- NULL
  class(value) <- "numeric"
  print(value)
  h0 <- attr(x, "h0")
  cat("H0:tau =", h0)
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
  # Error handling
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)
  new_t <- adjusted_t.numeric(estimate = estimate, r2yz.dx = r2yz.dx, r2dz.x = r2dz.x, se = se, dof = dof, reduce =  reduce, h0 = h0)
  partial_r2(t_statistic = new_t, dof = dof - 1)
}

#' @rdname adjusted_estimate
#' @export
adjusted_partial_r2.lm <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  with(model_data, adjusted_partial_r2.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
                              r2yz.dx = r2yz.dx, reduce = reduce, h0 = h0))
}

#' @rdname adjusted_estimate
#' @export
adjusted_partial_r2.fixest <- function(model, treatment,  r2dz.x, r2yz.dx, reduce = TRUE, h0 = 0, ...){
  # extract model data
  model_data <- model_helper.fixest(model, covariates = treatment)
  with(model_data, adjusted_partial_r2.numeric(estimate = estimate, se = se, dof = dof, r2dz.x = r2dz.x,
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
  # Error handling
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)

  # Run formula for bias in R^2 [14 in "Making Sense of Sensitivity"]
  bias <- bf(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx) * se * sqrt(dof)
  bias <- unname(bias)
  return(bias)
}



#' @rdname adjusted_estimate
#' @export
bias.lm <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper.lm(model, covariates = treatment)
  bias <- with(model_data, bias.numeric(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(bias) <- rep(treatment, length(bias))
  return(bias)
}

#' @rdname adjusted_estimate
#' @export
bias.fixest <- function(model, treatment,  r2dz.x, r2yz.dx, ...){
  # extract model data
  model_data <- model_helper.fixest(model, covariates = treatment)
  bias <- with(model_data, bias.numeric(se = se, dof = dof, r2dz.x = r2dz.x, r2yz.dx = r2yz.dx))
  names(bias) <- rep(treatment, length(bias))
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
  model_data <- model_helper.lm(model, covariates = treatment)
  rel.bias   <- with(model_data, relative_bias.numeric(estimate = estimate,
                                               se = se, dof = dof,
                                               r2dz.x = r2dz.x,
                                               r2yz.dx = r2yz.dx))
  names(rel.bias) <- rep(treatment, length(rel.bias))
  return(rel.bias)
}

#' @rdname adjusted_estimate
#' @export
relative_bias.fixest <- function(model, treatment, r2dz.x, r2yz.dx, ...){
  model_data <- model_helper.fixest(model, covariates = treatment)
  rel.bias   <- with(model_data, relative_bias.numeric(estimate = estimate,
                                               se = se, dof = dof,
                                               r2dz.x = r2dz.x,
                                               r2yz.dx = r2yz.dx))
  names(rel.bias) <- rep(treatment, length(rel.bias))
  return(rel.bias)
}


#' @rdname adjusted_estimate
#' @export
relative_bias.numeric <- function(estimate, se, dof, r2dz.x, r2yz.dx,  ...) {

  # Error handling
  check_se(se = se)
  check_dof(dof = dof)
  check_r2(r2yz.dx = r2yz.dx, r2dz.x =  r2dz.x)

  t_statistic <- abs(estimate/se)
  f <- partial_f(t_statistic = t_statistic, dof = dof)
  BF <- bf(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  q <- BF/f
  return(q)
}


#' @rdname adjusted_estimate
#' @param r.est restricted estimate. A numerical vector.
#' @param est unrestricted estimate. A numerical vector.
#'@export
rel_bias <- function(r.est, est){
  (r.est - est)/r.est
}



# Confounder Strength -----------------------------------------------------

bf <- function(r2dz.x, r2yz.dx){
  BF <- sqrt(r2yz.dx * r2dz.x / (1 - r2dz.x))
  return(BF)
}

sef <- function(r2dz.x, r2yz.dx){
  SEF <- sqrt(1 - r2yz.dx)/sqrt(1 - r2dz.x)
  return(SEF)
}






