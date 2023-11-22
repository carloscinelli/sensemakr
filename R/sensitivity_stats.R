# General Sensitivity Statistics ------------------------------------------

# (extreme) robustness value --------------------------------------------------------


#' Computes the (extreme) robustness value
#'
#' @description
#' This function computes the (extreme) robustness value of a regression coefficient.
#'
#' The extreme robustness value describes the minimum strength of association (parameterized in terms of partial R2) that
#' omitted variables would need to have with the treatment alone in order to change the estimated coefficient by
#' a certain amount (for instance, to bring it down to zero).
#'
#' The robustness value describes the minimum strength of association (parameterized in terms of partial R2) that
#' omitted variables would need to have \emph{both} with the treatment and with the outcome to change the estimated coefficient by
#' a certain amount (for instance, to bring it down to zero).
#'
#' For instance, a robustness value of 1\% means that an unobserved confounder that explain 1\% of the residual variance of the outcome
#' and 1\% of the residual variance of the treatment is strong enough to explain away the estimated effect. Whereas a robustness value of 90\%
#' means that any unobserved confounder that explain less than 90\% of the residual variance of both the outcome and the treatment assignment cannot
#' fully account for the observed effect. You may also compute robustness value taking into account sampling uncertainty. See details in Cinelli and Hazlett (2020).
#'
#' The functions \link{robustness_value} and \link{extreme_robustness_value} can take as input an \code{\link{lm}} object or you may directly pass the t-value and degrees of freedom.
#'
#'
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model or a \code{fixest} model with the
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
#' robustness_value(model, covariates = "directlyharmed", alpha = 1)
#'
#' ## extreme robustness value of directly harmed q =1 (reduce estimate to zero)
#' extreme_robustness_value(model, covariates = "directlyharmed", alpha = 1)
#'
#' ## note it equals the partial R2 of the treatment with the outcome
#' partial_r2(model, covariates = "directlyharmed")
#'
#' ## robustness value of directly harmed q = 1/2 (reduce estimate in half)
#' robustness_value(model, covariates = "directlyharmed", q = 1/2, alpha = 1)
#'
#' ## robustness value of directly harmed q = 1/2, alpha = 0.05
#' ## (reduce estimate in half, with 95% confidence)
#' robustness_value(model, covariates = "directlyharmed", q = 1/2, alpha = 0.05)
#'
#' # you can also provide the statistics directly
#' robustness_value(t_statistic = 4.18445, dof = 783, alpha = 1)
#'
#' extreme_robustness_value(t_statistic = 4.18445, dof = 783, alpha = 1)
#'
#' @return
#' The function returns a numerical vector with the robustness value. The arguments q and alpha are saved as attributes of the vector for reference.
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @export
#' @importFrom stats df.residual qt update vcov
robustness_value = function(model, ...) {
  UseMethod("robustness_value")
}

#' @param model an \code{lm} object with the regression model.
#' @param covariates model covariates for which the robustness value will be computed. Default is to compute
#' the robustness value of all covariates.
#' @param q percent change of the effect estimate that would be deemed problematic.  Default is \code{1},
#' which means a reduction of 100\% of the current effect estimate (bring estimate to zero). It has to be greater than zero.
#' @param alpha significance level.
#' @rdname robustness_value
#' @export
#' @importFrom stats setNames
robustness_value.lm = function(model,
                               covariates = NULL,
                               q = 1,
                               alpha = 0.05, ...) {

  # check arguments
  check_q(q)
  check_alpha(alpha)

  # extract model data
  model_data <- model_helper.lm(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # compute rv
  robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}

#' @param model an \code{fixest} object with the regression model.
#' @param covariates model covariates for which the robustness value will be computed. Default is to compute
#' the robustness value of all covariates.
#' @param q percent change of the effect estimate that would be deemed problematic.  Default is \code{1},
#' which means a reduction of 100\% of the current effect estimate (bring estimate to zero). It has to be greater than zero.
#' @param alpha significance level.
#' @param message should messages be printed? Default = TRUE.
#' @rdname robustness_value
#' @export
#' @importFrom stats setNames
robustness_value.fixest = function(model,
                               covariates = NULL,
                               q = 1,
                               alpha = 0.05,
                               message = TRUE,
                               ...) {

  # check arguments
  check_q(q)
  check_alpha(alpha)
  if(message){
    if(alpha <1){
      message_vcov.fixest(model)
    }
  }
  # extract model data
  model_data <- model_helper.fixest(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # compute rv
  robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}

robustness_value.default = function(model, ...) {
  stop("The `robustness_value` function must be passed either an `lm` model object, a `fixest` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

#' @param t_statistic \code{numeric} vector with the t-value of the coefficient estimates
#' @param  dof residual degrees of freedom of the regression
#' @rdname robustness_value
#' @export
robustness_value.numeric <- function(t_statistic, dof, q =1, alpha = 0.05, ...){

  # check arguments
  check_q(q)
  check_alpha(alpha)


  # computes fq
  fq  <-  q * abs(t_statistic / sqrt(dof))

  # computes critical f
  f.crit <- abs(qt(alpha / 2, df = dof - 1)) / sqrt(dof - 1)

  # computes fqa
  fqa <- fq - f.crit

  # constraint binding case
  rv  <-  0.5 * (sqrt(fqa^4 + (4 * fqa^2)) - fqa^2)

  # constraint not binding case
  rvx <- (fq^2 - f.crit^2)/(1 + fq^2)

  # combine results
  rv.out <- rv # initiate everyone as binding
  rv.out[fqa < 0] <- 0 # zero for those who have negative fqa
  rv.out[fqa > 0 & fq > 1/f.crit] <- rvx[fqa > 0 & fq > 1/f.crit] # extreme rv for those who are not binding

  attributes(rv.out) <- list(names = names(rv.out), q = q, alpha = alpha, class = c("numeric","rv"))
  rv.out
}

#
# #' @export
# robustness_value.sensemakr <- function(x, ...){
#   x$sensitivity_stats[,c("rv_q","rv_qa")]
# }


#' @export
#' @rdname robustness_value
extreme_robustness_value = function(model, ...) {
  UseMethod("extreme_robustness_value")
}

#' @export
#' @rdname robustness_value
extreme_robustness_value.lm = function(model,
                                       covariates = NULL,
                                       q = 1,
                                       alpha = 0.05, ...) {

  # check arguments
  check_q(q)
  check_alpha(alpha)

  # extract model data
  model_data <- model_helper.lm(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # compute rv
  extreme_robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}

#' @export
#' @rdname robustness_value
extreme_robustness_value.fixest = function(model,
                                   covariates = NULL,
                                   q = 1,
                                   alpha = 0.05,
                                   message = TRUE,
                                   ...) {

  # check arguments
  check_q(q)
  check_alpha(alpha)
  if(message){
    if(alpha < 1){
      message_vcov.fixest(model)
    }
  }
  # extract model data
  model_data <- model_helper.fixest(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # compute rv
  extreme_robustness_value(t_statistic = t_statistic, dof = dof, q = q, alpha = alpha)

}

#' @export
#' @rdname robustness_value
robustness_value.default = function(model, ...) {
  stop("The `extreme_robustness_value` function must be passed either an `lm` model object, a `fixest` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}


#' @export
#' @rdname robustness_value
extreme_robustness_value.numeric <- function(t_statistic, dof, q =1, alpha = 0.05, ...){

  # check arguments
  check_q(q)
  check_alpha(alpha)


  # computes fq
  fq  <-  q * abs(t_statistic / sqrt(dof))

  # computes critical f
  f.crit <- abs(qt(alpha / 2, df = dof - 1)) / sqrt(dof - 1)

  xrv <- (fq^2 - f.crit^2)/(1 + fq^2)

  attributes(xrv) <- list(names = names(xrv), q = q, alpha = alpha, class = c("numeric","rv"))
  xrv
}



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
#' For details see Cinelli and Hazlett (2020).
#'
#' The partial (Cohen's) f2 is a common measure of effect size (a transformation of the partial R2) that can also be used directly
#' for sensitivity analysis using a bias factor table.
#'
#' The function \code{partial_r2} computes the partial R2. The function \code{partial_f2} computes the partial f2 and the function \code{partial_f} the partial f.
#' They can take as input an \code{\link{lm}} object or you may pass directly t-value and degrees of freedom.
#'
#' For partial R2 of groups of covariates, check \code{\link{group_partial_r2}}.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model or a \code{fixest} model with the
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
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @export
partial_r2 = function(model, ...) {
  UseMethod("partial_r2")
}


#' @param model an \code{lm} object with the regression model
#' @param covariates model covariates for which the partial R2 will be computed. Default is to compute
#' the partial R2 of all covariates.
#' @rdname partial_r2
#' @export
partial_r2.lm = function(model, covariates = NULL, ...) {

  # extract model data
  model_data <- model_helper.lm(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_r2(t_statistic = t_statistic, dof = dof)
}

#' @param model an \code{fixest} object with the regression model
#' @param covariates model covariates for which the partial R2 will be computed. Default is to compute
#' the partial R2 of all covariates.
#' @rdname partial_r2
#' @export
partial_r2.fixest = function(model, covariates = NULL, ...) {

  # extract model data
  model_data <- model_helper.fixest(model, covariates = covariates)
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
  stop("The `partial_r2` function must be passed either an `lm`/`fixest` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}

# partial f2 --------------------------------------------------------------

#' @rdname partial_r2
#' @export
partial_f2 = function(model, ...) {
  UseMethod("partial_f2")
}

#' @rdname partial_r2
#' @export
partial_f2.numeric <- function(t_statistic, dof, ...){
  t_statistic^2 / dof
}

#' @rdname partial_r2
#' @export
partial_f2.lm <- function(model, covariates = NULL, ...) {
  # extract model data
  model_data <- model_helper.lm(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_f2(t_statistic = t_statistic, dof = dof)
}

#' @rdname partial_r2
#' @export
partial_f2.fixest = function(model, covariates = NULL, ...) {
  # extract model data
  model_data <- model_helper.fixest(model, covariates = covariates)
  t_statistic = setNames(model_data$t_statistics, model_data$covariates)
  dof         = model_data$dof

  # Return R^2 -- this is one R^2 for each coefficient, we will subset for
  # coeff of interest later.
  partial_f2(t_statistic = t_statistic, dof = dof)
}
partial_r2.default = function(model, ...) {
  stop("The `partial_f2` function must be passed either an `lm`/`fixest`  model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}



# partial f ---------------------------------------------------------------


#' @rdname partial_r2
#' @export
partial_f = function(model, ...) {
  UseMethod("partial_f")
  }

partial_f.fixest <- function(model, covariates = NULL, ...){
  sqrt(partial_f2.fixest(model, covariates = NULL, ...))
}

partial_f.lm <- function(model, covariates = NULL, ...) {
  sqrt(partial_f2.lm(model, covariates = NULL, ...))
}

partial_f.numeric <- function(t_statistic, dof, ...) {
  sqrt(partial_f2.numeric(t_statistic, dof, ...))
}



partial_f.numeric <- function(t_statistic, dof, ...) sqrt(partial_f2.numeric(t_statistic, dof, ...))

partial_r2.default = function(model, ...) {
  stop("The `partial_f` function must be passed either an `lm`/`fixest` model object, ",
       "or the t-statistics and degrees of freedom directly. ",
       "Other object types are not supported. The object passed was of class ",
       class(model)[1])
}




# group_partial_r2 --------------------------------------------------------


#' Partial R2 of groups of covariates in a linear regression model
#'
#' This function computes the partial R2 of a group of covariates in a linear regression model.
#'
#' @param ... arguments passed to other methods. First argument should either be an \code{lm} model or a \code{fixest} model with the
#' regression model or a numeric vector with the F-statistics for the group of covariates.
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
group_partial_r2 <- function(model, ...){
  UseMethod("group_partial_r2")
}




#' @inheritParams partial_r2
#' @param covariates model covariates for which their grouped partial R2 will be computed.
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

#' @inheritParams partial_r2
#' @param covariates model covariates for which their grouped partial R2 will be computed.
#' @rdname group_partial_r2
#' @export
group_partial_r2.fixest <- function(model, covariates, ...){

  if (missing(covariates)) stop("Argument covariates missing.")

  coefs <- coef(model)

  check_covariates(all_names = names(coefs), covariates = covariates)

  # coefficiens
  coefs <- coefs[covariates]

  # vcov matrix
  V <- vcov(model, vcov = "iid")[covariates, covariates, drop = FALSE]

  # degrees of freedom
  dof <- fixest::degrees_freedom(model, type = "resid", vcov = "iid")

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
#' \code{\link{partial_r2}} and \code{\link{partial_f2}} of the coefficient of interest.
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
#' A \code{data.frame} containing the following quantities:
#' \describe{
#' \item{treatment}{a character with the name of the treatment variable}
#' \item{estimate}{a numeric vector with the estimated effect of the treatment}
#' \item{se}{a numeric vector with  the estimated standard error of the treatment effect}
#' \item{t_statistics}{a numeric vector with  the t-value of the treatment}
#' \item{r2yd.x}{a numeric vector with  the partial R2 of the treatment and the outcome, see details in \code{\link{partial_r2}}}
#' \item{rv_q}{a numeric vector with  the robustness value of the treatment, see details  in \code{\link{robustness_value}}}
#' \item{rv_qa}{a numeric vector with the robustness value of the treatment considering statistical significance, see details  in \code{\link{robustness_value}}}
#' \item{f2yd.x }{a numeric vector with the partial (Cohen's) f2 of the treatment with the outcome, see details in \code{\link{partial_f2}}}
#' \item{dof}{a numeric vector with the degrees of freedom of the model}
#' }
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @export
sensitivity_stats <- function(model, ...){
  UseMethod("sensitivity_stats")
}

#' @inheritParams adjusted_estimate
#' @inheritParams robustness_value
#' @rdname sensitivity_stats
#' @export
sensitivity_stats.lm <- function(model,
                                 treatment,
                                 q = 1,
                                 alpha = 0.05,
                                 reduce = TRUE,
                                 ...)
{

  model_data <- model_helper.lm(model, covariates = treatment)
  sensitivity_stats <- with(model_data, sensitivity_stats(estimate = estimate,
                                                          se = se,
                                                          dof = dof,
                                                          treatment = treatment,
                                                          q = q,
                                                          alpha = alpha,
                                                          reduce = reduce,
                                                          ...))
  sensitivity_stats
}

#' @inheritParams adjusted_estimate
#' @inheritParams robustness_value
#' @rdname sensitivity_stats
#' @param message should messages be printed? Default = TRUE.
#' @export
sensitivity_stats.fixest <- function(model,
                                 treatment,
                                 q = 1,
                                 alpha = 0.05,
                                 reduce = TRUE,
                                 message = T,
                                 ...)
{
  if(message) message_vcov.fixest(model)

  model_data <- model_helper.fixest(model, covariates = treatment)
  sensitivity_stats <- with(model_data, sensitivity_stats(estimate = estimate,
                                                          se = se,
                                                          dof = dof,
                                                          treatment = treatment,
                                                          q = q,
                                                          alpha = alpha,
                                                          reduce = reduce,
                                                          ...))
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
                                      reduce = TRUE,
                                      ...)
{
  check_se(se)
  check_dof(dof)
  h0 <- ifelse(reduce, estimate*(1 - q), estimate*(1 + q) )
  original_t  <- estimate/se
  t_statistic <- (estimate - h0)/se
  sensitivity_stats <- data.frame(treatment = treatment, stringsAsFactors = FALSE)
  sensitivity_stats[["estimate"]] <- estimate
  sensitivity_stats[["se"]] <- se
  sensitivity_stats[["t_statistic"]] <- t_statistic
  sensitivity_stats[["r2yd.x"]] <- as.numeric(partial_r2(t_statistic = original_t, dof = dof))
  sensitivity_stats[["rv_q"]] <- (robustness_value(t_statistic = original_t, dof = dof, q = q, alpha = 1))
  sensitivity_stats[["rv_qa"]] <- (robustness_value(t_statistic = original_t, dof = dof, q = q, alpha = alpha))
  sensitivity_stats[["f2yd.x"]] <- as.numeric(partial_f2(t_statistic = original_t, dof = dof))
  sensitivity_stats[["dof"]] <- dof
  sensitivity_stats
}


# sanity checkers ---------------------------------------------------------


check_q <- function(q) {
  # Error: q non-numeric or out of bounds
  if (!is.numeric(q) || length(q) > 1 || q < 0) {
    stop("The `q` parameter must be a single number greater than 0.")
  }
}


check_alpha <- function(alpha) {
  # Error: alpha, if provided, was non-numeric or out of bounds
  if ((!is.numeric(alpha) || length(alpha) > 1 ||
                          alpha < 0 || alpha > 1)) {
    stop("`alpha` must be between 0 and 1.")
  }
}


check_se <- function(se){
  if (!is.numeric(se)) stop("Standard Error must be a numeric value")
  if (se < 0) stop("Standard error provided must be a single non-negative number")
}

check_dof <- function(dof){
    if (any(!is.numeric(dof) | dof < 0)) {
      stop("Degrees of freedom provided must be a non-negative number.")
    }
}





# model helpers -----------------------------------------------------------


#' Helper function for extracting model statistics
#'
#' This is an internal function used for extracting the necessary statistics from the models.
#'
#' @param model model to extract statistics from
#' @param covariates model covariates from which statistics will be extracted.
#' @param ... arguments passed to other methods.
#' @export
#' @keywords internal
model_helper = function(model, covariates = NULL, ...) {
  UseMethod("model_helper", model)
}

# model_helper.default = function(model, covariates = NULL) {
#   stop("The `partial_r2` function must be passed an `lm` model object. ",
#        "Other object types are not supported. The object passed was of class ",
#        class(model)[1])
# }

#' @export
model_helper.lm = function(model, covariates = NULL, ...) {
  # Quickly extract things from an lm object

  # If we have a dropped coefficient (multicolinearity), we're not going to
  # get an R^2 for this coefficient.
  # warn_na_coefficients(model, covariates = covariates)

  # Let's avoid the NaN problem from dividing by zero
  error_if_no_dof.lm(model)

  coefs <- coef(summary(model))
  covariates <- check_covariates(rownames(coefs), covariates)

  if (!is.null(covariates)) {coefs <- coefs[covariates, ,drop = FALSE]}

  return(list(
    covariates = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se = coefs[, "Std. Error"],
    t_statistics = coefs[, "t value"],
    dof = model$df.residual
  ))
}

#' @export
#' @keywords internal
model_helper.fixest = function(model, covariates = NULL, ...) {
  # Quickly extract things from an fixest object

  # If we have a dropped coefficient (multicolinearity), we're not going to
  # get an R^2 for this coefficient.
  # warn_na_coefficients(model, covariates = covariates)

  # Let's avoid the NaN problem from dividing by zero
  error_if_no_dof.fixest(model)


  coefs <- as.matrix(summary(model, vcov = "iid")$coeftable)
  covariates <- check_covariates(rownames(coefs), covariates)

  if (!is.null(covariates)) coefs <- coefs[covariates, ,drop = FALSE]

  return(list(
    covariates = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se = coefs[, "Std. Error"],
    t_statistics = coefs[, "t value"],
    dof = fixest::degrees_freedom(model, type = "resid", vcov = "iid")
  ))
}


# warn_na_coefficients = function(model, covariates = NULL) {
#
#   coefs <- coef(model)
#   covariates <- check_covariates(names(coefs), covariates)
#
#   if (!is.null(covariates))  coefs <- coefs[covariates]
#
#   if (any(is.na(coefs))) {
#     na_coefficients = names(coefs)[which(is.na(coefs))]
#     coefficient_string = paste(na_coefficients, collapse = ", ")
#     coefficient_string_plural = ifelse(length(na_coefficients) > 1,
#                                        "coefficients",
#                                        "coefficient")
#     warning("\n Model contains 'NA' ", coefficient_string_plural, ". Computations were not ",
#             "performed for coefficient: ", coefficient_string)
#   }
# }

#' Helper function for checking dof statistics
#'
#' This is an internal function used for checking dof statistics.
#'
#' @param ... arguments passed to other methods.
#' @export
#' @keywords internal
error_if_no_dof = function(model, ...) {
  UseMethod("error_if_no_dof")
}

#' Helper function for checking dof statistics
#'
#' This is an internal function used for checking dof statistics.
#'
#' @param model model to check dof in
#' @param ... arguments passed to other methods.
#' @keywords internal
error_if_no_dof.lm = function(model, ...) {
  if (model$df.residual == 0) {
    stop("There are 0 residual ",
         "degrees of freedom in the regression model provided.")
  }
}

#' Helper function for checking dof statistics
#'
#' This is an internal function used for checking dof statistics.
#'
#' @param model model to check dof in
#' @param ... arguments passed to other methods.
#' @keywords internal
error_if_no_dof.fixest = function(model, ...) {
  if (fixest::degrees_freedom(model, type = "resid", vcov = "iid") == 0) {
    stop("There are 0 residual ",
         "degrees of freedom in the regression model provided.")
  }
}



message_vcov.fixest <- function(model){
  vcov_type <- attr(summary(model)$coeftable, which = "type")
  if(!is.null(vcov_type)){
    if(vcov_type != "IID"){
      message("Note for fixest: using 'iid' standard errors. Support for robust standard errors coming soon.")
    }
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
