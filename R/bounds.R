
# Bounds ------------------------------------------------------------------





# future bounds to work on ------------------------------------------------

total_r2_bound <- function(r2dxj, r2yxj, r2dx, r2yx, r2yd.x, kd = 1, ky = 1){

  check_k(ky)
  check_k(kd)
  if (length(r2dxj) != length(r2yxj)) stop("r2dxj and r2yxj must have the same length")
  if (length(r2dx) > 1) stop("r2dx must be of length 1")
  if (length(r2yx) > 1) stop("r2yx must be of length 1")
  if (length(r2yd.x) > 1) stop("r2yd.x must be of length 1")

  r2dz.x = kd*(r2dxj/(1 - r2dx))
  r2yz.x = ky*(r2yxj/(1 - r2yx))
  r2yz.dx = ((sqrt(r2yz.x) - ssqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
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
  r2yz.dx = ((sqrt(r2yz.x) - ssqrt(r2yd.x)*sqrt(r2dz.x))/(sqrt(1 - r2yd.x)*sqrt(1 - r2dz.x)))^2
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

  out = data.frame(r2dz.x = r2dz.x,
                   r2yz.dx = r2yz.dx,
                   bound_label = bound_label,
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
ovb_bounds <- function(model,
                       treatment,
                       benchmark_covariates,
                       kd,
                       ky,
                       bound_type = c("partial r2", "partial r2 no D", "total r2")) {

  bound_type = match.arg(bound_type)

  bounder = switch(bound_type,
                   "partial r2" = ovb_partial_r2_bound,
                   "partial r2 no D" = 1,
                   "total r2" = 2)

  bounder(model = model,
          treatment = treatment,
          benchmark_covariates = benchmark_covariates,
          kd = kd,
          ky = ky)
}

label_maker <- function(benchmark_covariate, kd, ky) {
  # Generate the label text
  variable_text = ifelse(
    is.null(benchmark_covariate),
    "\n",
    paste0(" ", benchmark_covariate)
  )

  multiplier_text = ifelse(
    ky == kd,
    paste0(ky, "x"),
    paste0(kd, "/", ky,"x")
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
