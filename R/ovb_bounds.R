
# Bounds ------------------------------------------------------------------


# general bounder ---------------------------------------------------------

#  workhorse for any bounding type

#' Bounds on the strength of unobserved confounders using observed covariates
#'
#' @description
#' Bounds on the strength of unobserved confounders using observed covariates, as in Cinelli and Hazlett (2020). The main generic function is \code{ovb_bounds}, which can compute both the bounds on the strength of confounding as well as the adjusted estimates, standard errors, t-values and confidence intervals.
#'
#' Other functions that compute only the bounds on the strength of confounding are also provided. These functions may be useful when computing benchmarks for using only summary statistics from papers you see in print.
#'
#' @details
#' Currently it implements only the bounds based on partial R2. Other bounds will be implemented soon.
#'
#'
#' @return
#' The function \code{ovb_bounds} returns a \code{\link{data.frame}} with the bounds on the strength of the unobserved confounder
#' as well with the adjusted point estimates, standard errors and t-values (optional, controlled by argument \code{adjusted_estimates = TRUE}).
#'
#' @inheritParams sensemakr
#' @rdname ovb_bounds
#' @export
ovb_bounds <- function(...){
  UseMethod("ovb_bounds")
}

#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @param bound type of bounding procedure. Currently only \code{"partial r2"} is implemented.
#' @param adjusted_estimates should the bounder also compute the adjusted estimates? Default is \code{TRUE}.
#' @param alpha significance level for computing the adjusted confidence intervals. Default is 0.05.
#' @examples
#'
#'# runs regression model
#'model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#'# bounds on the strength of confounders 1, 2, or 3 times as strong as female
#'# and 1,2, or 3 times as strong as pastvoted
#'ovb_bounds(model, treatment = "directlyharmed",
#'           benchmark_covariates = c("female", "pastvoted"),
#'           kd = 1:3)
#'
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @rdname ovb_bounds
#' @export
ovb_bounds.lm <- function(model,
                          treatment,
                          benchmark_covariates = NULL,
                          kd = 1,
                          ky = kd,
                          reduce = TRUE,
                          bound = c("partial r2", "partial r2 no D", "total r2"),
                          adjusted_estimates = TRUE,
                          alpha = 0.05,
                          h0 = 0,
                          ...) {

  bound = match.arg(bound)

  bounder = switch(bound,
                   "partial r2" = ovb_partial_r2_bound,
                   "partial r2 no D" = stop("Only partial r2 implemented now."),
                   "total r2" = stop("Only partial r2 implemented now."))

  bounds <- bounder(model = model,
                    treatment = treatment,
                    benchmark_covariates = benchmark_covariates,
                    kd = kd,
                    ky = ky)

  if (adjusted_estimates) {
    # compute adjusted effects
    bounds$treatment <- treatment

    bounds$adjusted_estimate = adjusted_estimate.lm(model = model,
                                                 treatment = treatment,
                                                 r2yz.dx = bounds$r2yz.dx,
                                                 r2dz.x = bounds$r2dz.x,
                                                 reduce = reduce)

    bounds$adjusted_se = adjusted_se.lm(model = model,
                                     treatment = treatment,
                                     r2yz.dx = bounds$r2yz.dx,
                                     r2dz.x = bounds$r2dz.x)

    bounds$adjusted_t = adjusted_t.lm(model = model,
                                   treatment = treatment,
                                   r2yz.dx = bounds$r2yz.dx,
                                   r2dz.x = bounds$r2dz.x,
                                   h0 = h0,
                                   reduce = reduce)

    se_multiple <- qt(alpha/2, df = model$df.residual, lower.tail = F)
    bounds$adjusted_lower_CI <- bounds$adjusted_estimate - se_multiple*bounds$adjusted_se
    bounds$adjusted_upper_CI <- bounds$adjusted_estimate + se_multiple*bounds$adjusted_se


  }
  class(bounds) <- c("ovb_bounds", "data.frame")
  row.names(bounds) <- NULL
  return(bounds)
}

#' @inheritParams sensemakr
#' @inheritParams adjusted_estimate
#' @param bound type of bounding procedure. Currently only \code{"partial r2"} is implemented.
#' @param adjusted_estimates should the bounder also compute the adjusted estimates? Default is \code{TRUE}.
#' @param alpha significance level for computing the adjusted confidence intervals. Default is 0.05.
#' @examples
#'
#'# run regression model
#'model <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'              pastvoted + hhsize_darfur + female + village, data = darfur)
#'
#'# bounds on the strength of confounders 1, 2, or 3 times as strong as female
#'# and 1,2, or 3 times as strong as pastvoted
#'ovb_bounds(model, treatment = "directlyharmed",
#'           benchmark_covariates = c("female", "pastvoted"),
#'           kd = 1:3)
#'
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @rdname ovb_bounds
#' @param message should messages be printed? Default = TRUE.
#' @export
ovb_bounds.fixest <- function(model,
                          treatment,
                          benchmark_covariates = NULL,
                          kd = 1,
                          ky = kd,
                          reduce = TRUE,
                          bound = c("partial r2", "partial r2 no D", "total r2"),
                          adjusted_estimates = TRUE,
                          alpha = 0.05,
                          h0 = 0,
                          message = T,
                          ...) {

  bound = match.arg(bound)

  bounder = switch(bound,
                   "partial r2" = ovb_partial_r2_bound,
                   "partial r2 no D" = stop("Only partial r2 implemented now."),
                   "total r2" = stop("Only partial r2 implemented now."))

  bounds <- bounder(model = model,
                    treatment = treatment,
                    benchmark_covariates = benchmark_covariates,
                    kd = kd,
                    ky = ky)

  if (adjusted_estimates) {
    # compute adjusted effects
    bounds$treatment <- treatment

    bounds$adjusted_estimate = adjusted_estimate.fixest(model = model,
                                                 treatment = treatment,
                                                 r2yz.dx = bounds$r2yz.dx,
                                                 r2dz.x = bounds$r2dz.x,
                                                 reduce = reduce)

    bounds$adjusted_se = adjusted_se.fixest(model = model,
                                     treatment = treatment,
                                     r2yz.dx = bounds$r2yz.dx,
                                     r2dz.x = bounds$r2dz.x,
                                     message = message)

    bounds$adjusted_t = adjusted_t.fixest(model = model,
                                   treatment = treatment,
                                   r2yz.dx = bounds$r2yz.dx,
                                   r2dz.x = bounds$r2dz.x,
                                   h0 = h0,
                                   reduce = reduce,
                                   message = message)

    se_multiple <- qt(alpha/2, df = fixest::degrees_freedom(model, type = "resid", vcov = "iid"), lower.tail = F)
    bounds$adjusted_lower_CI <- bounds$adjusted_estimate - se_multiple*bounds$adjusted_se
    bounds$adjusted_upper_CI <- bounds$adjusted_estimate + se_multiple*bounds$adjusted_se


  }
  class(bounds) <- c("ovb_bounds", "data.frame")
  row.names(bounds) <- NULL
  return(bounds)
}


# future bounds to work on ------------------------------------------------


# total_r2_bound <- function(r2dxj, r2yxj, r2dx, r2yx, r2yd.x, kd = 1, ky = 1){
#
#   check_k(ky)
#   check_k(kd)
#   if (length(r2dxj) != length(r2yxj)) stop("r2dxj and r2yxj must have the same length")
#   if (length(r2dx) > 1) stop("r2dx must be of length 1")
#   if (length(r2yx) > 1) stop("r2yx must be of length 1")
#   if (length(r2yd.x) > 1) stop("r2yd.x must be of length 1")
#
#   r2dz.x = kd*(r2dxj/(1 - r2dx))
#   r2yz.x = ky*(r2yxj/(1 - r2yx))
#   r2yz.dx = ((sqrt(r2yz.x) - sqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
#   out = list(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
#   return(out)
# }
#
# partial_r2_bound_no_d <- function(r2dxj.x, r2yxj.dx, r2yd.x, kd = 1, ky = 1){
#
#   check_k(ky)
#   check_k(kd)
#   if (length(r2dxj.x) != length(r2yxj.dx)) stop("r2dxj.x and r2yxj.dx must have the same length")
#   if (length(r2yd.x) > 1) stop("r2yd.x must be of length 1")
#
#   r2dz.x = kd*(r2dxj.x/(1 - r2dxj.x))
#   r2yz.x = ky*(r2yxj.dx/(1 - r2yxj.dx))
#   r2yz.dx = ((sqrt(r2yz.x) - sqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
#   out = list(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
#   return(out)
# }



# bounding methods --------------------------------------------------------


#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
#' @rdname ovb_bounds
#' @export
ovb_partial_r2_bound <- function(...){
  UseMethod("ovb_partial_r2_bound")
}

#' @param r2dxj.x partial R2 of covariate Xj with the treatment D (after partialling out the effect of the remaining covariates X, excluding Xj).
#' @param r2yxj.dx partial R2 of covariate Xj with the outcome Y (after partialling out the effect of the remaining covariates X, excluding Xj).
#' @return
#' The function `ovb_partial_r2_bound()` returns only \code{\link{data.frame}} with the bounds on the strength of the unobserved confounder. Adjusted estimates, standard
#' errors and t-values (among other quantities) need to be computed manually by the user using those bounds with the functions \code{\link{adjusted_estimate}}, \code{\link{adjusted_se}} and \code{\link{adjusted_t}}.
#'
#' @examples
#'
#'#########################################################
#'## Let's construct bounds from summary statistics only ##
#'#########################################################
#'# Suppose you didn't have access to the data, but only to
#'# the treatment and outcome regression tables.
#'# You can still compute the bounds.
#'
#'# Use the t statistic of female in the outcome regression
#'# to compute the partial R2 of female with the outcome.
#'r2yxj.dx <- partial_r2(t_statistic = -9.789, dof = 783)
#'
#'# Use the t-value of female in the *treatment* regression
#'# to compute the partial R2 of female with the treatment
#'r2dxj.x <- partial_r2(t_statistic = -2.680, dof = 783)
#'
#'# Compute manually bounds on the strength of confounders 1, 2, or 3
#'# times as strong as female
#'bounds <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x,
#'                               r2yxj.dx = r2yxj.dx,
#'                               kd = 1:3,
#'                               ky = 1:3,
#'                               bound_label = paste(1:3, "x", "female"))
#'# Compute manually adjusted estimates
#'bound.values <- adjusted_estimate(estimate = 0.0973,
#'                                  se = 0.0232,
#'                                  dof = 783,
#'                                  r2dz.x = bounds$r2dz.x,
#'                                  r2yz.dx = bounds$r2yz.dx)
#'
#'# Plot contours and bounds
#'ovb_contour_plot(estimate = 0.0973,
#'                 se = 0.0232,
#'                 dof = 783)
#'add_bound_to_contour(bounds, bound_value = bound.values)
#'
#'
#' @inheritParams sensemakr
#' @inheritParams ovb_bounds
#' @rdname ovb_bounds
#' @export
#' @import stats
ovb_partial_r2_bound.numeric <- function(r2dxj.x,
                                         r2yxj.dx,
                                         kd = 1,
                                         ky = kd,
                                         bound_label = "manual",
                                         ...){
  # Error handling

  check_r2(r2yz.dx = r2dxj.x, r2dz.x =  r2yxj.dx)

  r2dz.x <- kd*(r2dxj.x/(1 - r2dxj.x))

  if (any(r2dz.x >= 1)) {
    stop("Implied bound on r2dz.x >= 1. Impossible kd value. Try a lower kd.")
  }

  # Bound for R^2 of Y with Z: Footnote 10
  r2zxj.xd = kd * (r2dxj.x^2) / ((1 - kd * r2dxj.x) * (1 - r2dxj.x))

  if (any(r2zxj.xd >= 1)) {
    stop("Impossible kd value. Try a lower kd.")
  }

  r2yz.dx <- ((sqrt(ky) + sqrt(r2zxj.xd)) /
               sqrt(1 - r2zxj.xd))^2 * (r2yxj.dx / (1 - r2yxj.dx))

  if (any(r2yz.dx > 1)) {
    warning("Implied bound on r2yz.dx greater than 1, try lower kd and/or ky. Setting r2yz.dx to 1.")
    r2yz.dx[r2yz.dx > 1] <- 1
  }

  out <- data.frame(bound_label = bound_label,
                    r2dz.x = r2dz.x,
                    r2yz.dx = r2yz.dx,
                    stringsAsFactors = FALSE)
  class(out) <- c("ovb_partial_r2_bound", "data.frame")
  return(out)
}

#' @inheritParams sensemakr
#' @inheritParams ovb_bounds
#' @rdname ovb_bounds
#' @export
#' @import stats
ovb_partial_r2_bound.lm <- function(model,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    ky = kd,
                                    adjusted_estimates = TRUE,
                                    alpha = 0.05, ...){

  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")



  # treatment model
  #treatment_model <- update(model,  paste(treatment, "~ . - ", treatment))

  m      <- model.matrix(model)
  keep   <- !(colnames(m) %in% treatment)
  d      <- m[,treatment]
  XX     <- m[, keep, drop = FALSE]
  treatment_model <- lm(d ~ XX + 0)
  treatment_model

  # treatment_model <- lm.fit(y = m[,treatment, drop = F], x = m[,keep])
  # quoted <- sapply(colnames(m[,keep]), function(x) paste0("`", x, "`"))
  # vars   <- paste(quoted, collapse = " + ")
  #form1   <- paste(treatment, "~", vars)
  #form   <- eval(parse(text = form1, keep.source = T))
  #treatment_model <- lm(as.formula(form), data = as.data.frame(m))
  # initialize

  bounds <- vector(mode = "list", length = length(benchmark_covariates))
  r2yxj.dx <- NULL
  r2dxj.x  <- NULL

  if (!is.null(benchmark_covariates)) {
    if(is.character(benchmark_covariates)){

      # gets partial r2 with outcome
      r2yxj.dx <- partial_r2.lm(model, covariates = benchmark_covariates)

      # gets partial r2 with treatment
      bench.treat  <- paste0("XX", benchmark_covariates)
      r2dxj.x <- partial_r2.lm(treatment_model, covariates = bench.treat)

    } else {

      if (!is.list(benchmark_covariates)) stop("Argument benchmark_covariates must be either a string or a list.")

      if (is.null(names(benchmark_covariates)))
      stop("If benchmark_covariates is a list, it must be a *named* list.\t",
            "Names are used for labeling the benchmarks.")

      classes <- sapply(X = benchmark_covariates, FUN = class)

      if(!all(classes == "character"))
        stop("All elements of the list passed to benchmark_covariates must be of class 'character'.")

      n_groups <- length(benchmark_covariates)
      label.groups <- r2d.group <- r2y.group <- rep(NA, n_groups)

      for(i in seq_along(benchmark_covariates)){
        r2y.group[i] <-  group_partial_r2.lm(model, covariates = benchmark_covariates[[i]])
        bench.treat  <- paste0("XX", benchmark_covariates[[i]])
        r2d.group[i] <-  group_partial_r2.lm(treatment_model, covariates = bench.treat)
        label.groups[i] <- names(benchmark_covariates)[i]
      }

      r2yxj.dx <- r2y.group
      r2dxj.x  <- r2d.group
      benchmark_covariates <-  label.groups
    }

  }

  for (i in seq_along(benchmark_covariates)) {

    bound_label = label_maker(benchmark_covariate = benchmark_covariates[i],
                              kd = kd,
                              ky = ky)

    bounds[[i]] <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x[i],
                                        r2yxj.dx = r2yxj.dx[i],
                                        kd = kd,
                                        ky = ky,
                                        bound_label = bound_label)
  }

  bounds <- do.call("rbind", bounds)

  return(bounds)
}


#' @inheritParams sensemakr
#' @inheritParams ovb_bounds
#' @rdname ovb_bounds
#' @export
#' @import stats
ovb_partial_r2_bound.fixest <- function(model,
                                    treatment,
                                    benchmark_covariates = NULL,
                                    kd = 1,
                                    ky = kd,
                                    adjusted_estimates = TRUE,
                                    alpha = 0.05, ...){

  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")

  # treatment model
  #treatment_model <- update(model, update.formula(model$fml, paste(treatment, "~ . - ", treatment)))

  m      <- model.matrix(model)
  keep   <- !(colnames(m) %in% treatment)
  d      <- as.matrix(m[,treatment])
  colnames(d) <- treatment
  XX     <- m[, keep, drop = FALSE]

  # vcov <- model$call$vcov

  if (!is.null(model$fixef_id)) {
    fixef_df <- data.frame(model$fixef_id[1:length(model$fixef_id)])
    treatment_model <- fixest::feols.fit(y = d, X = XX, fixef_df = fixef_df)
  } else {
    treatment_model <- fixest::feols.fit(y = d, X = XX)
  }

  # treatment_model <- lm.fit(y = m[,treatment, drop = F], x = m[,keep])
  # quoted <- sapply(colnames(m[,keep]), function(x) paste0("`", x, "`"))
  # vars   <- paste(quoted, collapse = " + ")
  #form1   <- paste(treatment, "~", vars)
  #form   <- eval(parse(text = form1, keep.source = T))
  #treatment_model <- lm(as.formula(form), data = as.data.frame(m))
  # initialize

  bounds <- vector(mode = "list", length = length(benchmark_covariates))
  r2yxj.dx <- NULL
  r2dxj.x  <- NULL

  if (!is.null(benchmark_covariates)) {
    if(is.character(benchmark_covariates)){

      # gets partial r2 with outcome
      r2yxj.dx <- partial_r2.fixest(model, covariates = benchmark_covariates)

      # gets partial r2 with treatment
      #bench.treat  <- paste0("XX", benchmark_covariates)
      r2dxj.x <- partial_r2.fixest(treatment_model, covariates = benchmark_covariates)

    } else {

      if (!is.list(benchmark_covariates)) stop("Argument benchmark_covariates must be either a string or a list.")

      if (is.null(names(benchmark_covariates)))
        stop("If benchmark_covariates is a list, it must be a *named* list.\t",
             "Names are used for labeling the benchmarks.")

      classes <- sapply(X = benchmark_covariates, FUN = class)

      if(!all(classes == "character"))
        stop("All elements of the list passed to benchmark_covariates must be of class 'character'.")

      n_groups <- length(benchmark_covariates)
      label.groups <- r2d.group <- r2y.group <- rep(NA, n_groups)

      for(i in seq_along(benchmark_covariates)){
        r2y.group[i] <-  group_partial_r2.fixest(model, covariates = benchmark_covariates[[i]])
        #bench.treat  <- paste0("XX", benchmark_covariates[[i]])
        r2d.group[i] <-  group_partial_r2.fixest(treatment_model, covariates = benchmark_covariates[[i]])
        label.groups[i] <- names(benchmark_covariates)[i]
      }

      r2yxj.dx <- r2y.group
      r2dxj.x  <- r2d.group
      benchmark_covariates <-  label.groups
    }

  }

  for (i in seq_along(benchmark_covariates)) {

    bound_label = label_maker(benchmark_covariate = benchmark_covariates[i],
                              kd = kd,
                              ky = ky)

    bounds[[i]] <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x[i],
                                        r2yxj.dx = r2yxj.dx[i],
                                        kd = kd,
                                        ky = ky,
                                        bound_label = bound_label)
  }

  bounds <- do.call("rbind", bounds)

  return(bounds)
}



label_maker <- function(benchmark_covariate, kd, ky, digits = 2) {
  # Generate the label text
  variable_text = ifelse(
    is.null(benchmark_covariate),
    "\n",
    paste0(" ", benchmark_covariate)
  )

  multiplier_text = ifelse(
    ky == kd,
    paste0(round(ky, digits = digits)),
    paste0(round(kd, digits = digits), "/", round(ky, digits = digits))
  )

  bound_label = paste0(paste0(multiplier_text, "x"), variable_text)

  return(bound_label)

}



# sanity checkers ---------------------------------------------------------

# check_k <- function(k){
#   if (!is.numeric(k) || length(k) > 1 || k < 0) {
#     stop("`k` must be a single non-negative number.")
#   }
# }
