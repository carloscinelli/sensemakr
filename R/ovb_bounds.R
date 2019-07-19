
# Bounds ------------------------------------------------------------------





# future bounds to work on ------------------------------------------------

#' @importFrom stats df.residual qt update vcov
total_r2_bound <- function(r2dxj, r2yxj, r2dx, r2yx, r2yd.x, kd = 1, ky = 1){

  check_k(ky)
  check_k(kd)
  if (length(r2dxj) != length(r2yxj)) stop("r2dxj and r2yxj must have the same length")
  if (length(r2dx) > 1) stop("r2dx must be of length 1")
  if (length(r2yx) > 1) stop("r2yx must be of length 1")
  if (length(r2yd.x) > 1) stop("r2yd.x must be of length 1")

  r2dz.x = kd*(r2dxj/(1 - r2dx))
  r2yz.x = ky*(r2yxj/(1 - r2yx))
  r2yz.dx = ((sqrt(r2yz.x) - sqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
  out = list(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  return(out)
}

partial_r2_bound_no_d <- function(r2dxj.x, r2yxj.x, r2yd.x, kd = 1, ky = 1){

  check_k(ky)
  check_k(kd)
  if (length(r2dxj.x) != length(r2yxj.x)) stop("r2dxj.x and r2yxj.x must have the same length")
  if (length(r2yd.x) > 1) stop("r2yd.x must be of length 1")

  r2dz.x = kd*(r2dxj.x/(1 - r2dxj.x))
  r2yz.x = ky*(r2yxj.x/(1 - r2yxj.x))
  r2yz.dx = ((sqrt(r2yz.x) - sqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
  out = list(r2dz.x = r2dz.x, r2yz.dx = r2yz.dx)
  return(out)
}



# bounding methods --------------------------------------------------------


ovb_partial_r2_bound <- function(...){
  UseMethod("ovb_partial_r2_bound")
}

ovb_partial_r2_bound.numeric <- function(r2dxj.x,
                                         r2yxj.x,
                                         kd = 1,
                                         ky = 1,
                                         bound_label = ""){

  if(!is.numeric(r2dxj.x)||!is.numeric(r2yxj.x)) stop("r2dxj.x and r2yxj.x must be numeric")
  if (length(r2dxj.x) != length(r2yxj.x)) stop("r2dxj.x and r2yxj.x must have the same length")


  r2dz.x = kd*(r2dxj.x/(1 - r2dxj.x))

  if (any(r2dz.x > 1)) {
    warning("Impossible kd value, returning NA. Try a lower kd.")
    r2dz.x[r2dz.x > 1] <- NA
  }

  # Bound for R^2 of Y with Z: Footnote 10
  r2zxj.xd = kd * (r2dxj.x^2) / ((1 - kd * r2dxj.x) * (1 - r2dxj.x))

  if (any(r2zxj.xd > 1)) {
    warning("Impossible kd value, returning NA. Try a lower kd.")
    r2zxj.xd[r2zxj.xd > 1] <- NA
  }

  r2yz.dx = ((sqrt(ky) + sqrt(r2zxj.xd)) /
               sqrt(1 - r2zxj.xd))^2 * (r2yxj.x / (1 - r2yxj.x))

  if (any(r2yz.dx > 1)) {
    warning("Implied bound on r2yz.dx greater than 1, try lower kd and/or ky. Setting r2yz.dx to 1.")
    r2yz.dx[r2yz.dx > 1] <- 1
  }

  out = data.frame(
    bound_label = bound_label,
    r2dz.x = r2dz.x,
    r2yz.dx = r2yz.dx,
    stringsAsFactors = FALSE)

  return(out)
}

ovb_partial_r2_bound.lm <- function(model,
                                    treatment,
                                    benchmark_covariates,
                                    kd = 1, ky = kd){

  if (!is.character(treatment)) stop("Argument treatment must be a string.")
  if (length(treatment) > 1) stop("You must pass only one treatment")


  if (!is.character(benchmark_covariates)) stop("Argument benchmark_covariates must be a string.")

  bounds <- vector(mode = "list", length = length(benchmark_covariates))

  # gets partial r2 with outcome
  r2yxj.x <- partial_r2(model, covariates = benchmark_covariates)

  # gets partial r2 with treatment
  treatment_model <- update(model,  paste(treatment, "~ . - ", treatment))
  r2dxj.x <- partial_r2(treatment_model, covariates = benchmark_covariates)

  for (i in seq_along(benchmark_covariates)) {

    bound_label = label_maker(benchmark_covariate = benchmark_covariates[i],
                              kd = kd,
                              ky = ky)

    bounds[[i]] <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x[i],
                                        r2yxj.x = r2yxj.x[i],
                                        kd = kd,
                                        ky = ky,
                                        bound_label = bound_label)
  }
  bounds <- do.call("rbind", bounds)
  return(bounds)
}



# general bounder ---------------------------------------------------------

#  workhorse for any bounding type

#' Bounds on the strength of unobserved confounders using observed covariates
#'
#' @description
#' Bounds on the strength of unobserved confounders using observed covariates, as in Cinelli and Hazlett (2018).
#'
#' @details
#' Currently it implements only the bounds based on partial R2). Other bounds will be implemented soon.
#'
#'
#' @return
#' The function returns a \code{\link{data.frame}} with the bounds on the strength of the unobserved confounder
#' as well with the adjusted point estimates, standard errors and t-values (optional, controlled by argument \code{adjusted_estimates}).
#'
#' @inheritParams sensemakr
#' @export
ovb_bounds <- function(...){
  UseMethod("ovb_bounds")
}

#' @inheritParams ovb_contour_plot
#' @rdname ovb_bounds
#' @param bound type of bounding procedure. Currently only \code{"partial r2"} is implemented.
#' @param adjusted_estimates should the bounder also compute the adjusted estimates?
#' Default is \code{TRUE}.
#'
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
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#' @export
ovb_bounds.lm <- function(model,
                          treatment,
                          benchmark_covariates,
                          kd = 1,
                          ky = kd,
                          reduce = TRUE,
                          bound = c("partial r2", "partial r2 no D", "total r2"),
                          adjusted_estimates = TRUE,
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
    bounds$adjusted_estimate = adjusted_estimate(model = model,
                                                 treatment = treatment,
                                                 r2yz.dx = bounds$r2yz.dx,
                                                 r2dz.x = bounds$r2dz.x,
                                                 reduce = reduce)

    bounds$adjusted_se = adjusted_se(model = model,
                                     treatment = treatment,
                                     r2yz.dx = bounds$r2yz.dx,
                                     r2dz.x = bounds$r2dz.x)

    bounds$adjusted_t = adjusted_t(model = model,
                                   treatment = treatment,
                                   r2yz.dx = bounds$r2yz.dx,
                                   r2dz.x = bounds$r2dz.x,
                                   reduce = reduce)
  }
  class(bounds) <- c("ovb_bounds", "data.frame")
  bounds
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

  bound_label = paste0( multiplier_text, variable_text)

  return(bound_label)

}



# sanity checkers ---------------------------------------------------------

check_k <- function(k){
  if (!is.numeric(k) || length(k) > 1 || k < 0) {
    stop("`k` must be a single non-negative number.")
  }
}
