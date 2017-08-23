# the updated internal name change is not reflected here
# TODO: push the changes later

# prints ------------------------------------------------------------------

# A method must have all the arguments of the generic, including … if the generic does.
# A method must have arguments in exactly the same order as the generic.

##' @export
print.sensemade <- function(x, str = TRUE){

  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Outcome:", x$info$outcome, "\n\n")
  cat("Object content:\n")
  if (str) str(x, max.level = 1)
  cat("\n For more, use plot and summary.")
}


##' @export
summary.sensemade <- function(object, q=1,scenarios=c(1,.25)){
  # bunch of useful things
  # return list with several useful things
  # returns a obj of class summary.sensemade
  out <- object
  class(out) <- "summary.sensemade"
  print.summary.sensemade(out, q=q, scenarios=scenarios)
}

##' @export
interpret <- function(sensemade, q = 1){

  # from: sensemade$benchmarks$benchmark_eachvar$r2y
  # to: sensemade$benchmarks$benchmark_eachvar$r2y

  # make sure 'max()' is choosing right benchmark point
  # currently points at 'female' not 'village',
  # since 'village' is in (sense$benchmarks$benchmark_group)
  # currently, only looking at (sensemade$benchmarks$benchmark_eachvar)

  idxr2y <- which.max(sensemade$benchmarks$benchmark_eachvar$r2y)
  idxr2d <- which.max(sensemade$benchmarks$benchmark_eachvar$r2d)
  estimate <- sensemade$treat.stats$estimate
  se <- sensemade$treat.stats$se
  t <- estimate/se
  df <- sensemade$treat.stats$df
  varR2D <- sensemade$benchmarks$benchmark_eachvar$covariate[idxr2d]
  maxR2d <- sensemade$benchmarks$benchmark_eachvar$r2d[idxr2d]
  varR2Y <- sensemade$benchmarks$benchmark_eachvar$covariate[idxr2y]
  maxR2y <- sensemade$benchmarks$benchmark_eachvar$r2y[idxr2d]
  r2dc   <- t^2/(t^2 + (maxR2y/q^2)*df)
  r2yc   <- ((q*t)^2)*((1 - maxR2d)/(maxR2d*df))

  cat("\n")
  cat("---Using the covariate most strongly associated with the treatment assignment as a benchmark---\n\n")
  if (r2yc > 1) {

    maxBias <- (get_bias(se, df = df, r2d = maxR2d, r2y = 1))

    cat("An unobserved confounder explaining as much of the treatment as '",
        varR2D, "' (", round(maxR2d,3), ") ", " would be able to cause at most a bias of ",
        round(maxBias, 3)," with an adjusted treatment effect of ", round(adjust_estimate(estimate, maxBias), 3),
        " in the extreme case where the confouder explains all the residual variance of the outcome (R2y = 1).",
        sep = "")

  } else {

    cat("An unobserved confounder as associated with the treatment as '",
        varR2D, "' (R2d = ", round(maxR2d,3), ") ", " would have to be at least ", round(r2yc/maxR2y, 1),
        " times more strongly associated with the outcome"," (reaching R2y = ", round(r2yc,3), ") ",
        "in order to reduce the treatment effect by ", q*100, "%", sep = "")
  }
  cat("\n\n")
  cat("---Using the covariate most strongly associated with the outcome as a benchmark---\n\n")
  cat("An unobserved confounder as associated with the outcome as '",
      varR2Y, "' (R2y = ", round(maxR2y,3), ") ", " would have to be at least ", round(r2dc/maxR2d, 1),
      " times as strongly associated with the treatment"," (reaching R2d = ", round(r2dc, 3), ") ",
      "in order to reduce the treatment effect by ", q*100, "%", sep = "")
  cat("\n\n")
}

##' @export
worstcaseinterpret <- function(sensemade, scenarios = c(1, 0.25), q = 1){
  estimate <- sensemade$treat.stats$estimate
  se <- sensemade$treat.stats$se
  t <- estimate/se
  df <- sensemade$treat.stats$df
  r2dc   <- t^2/(t^2 + (scenarios/q^2)*df)

  cat("Considering the extreme scenarios of unobserved confounders explaining ",
  paste0(scenarios*100, "%", collapse = ", "), " of the residual variance of the outcome, they would have",
  " to, respectively, explain at least ", paste0(round(r2dc*100, 2), "%", collapse = ", "),
  " of the treatment assignment to reduce the treatment effect in ", round(q*100, 2), "%.", sep = "")
}

##' @export
print.summary.sensemade <- function(x, q = 1, scenarios = c(1,.25)){
 # pretty print for the summary
  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Outcome:", x$info$outcome, "\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Unadjusted Treatment Effect:", round(x$treat.stats$estimate, 3), "\n")
  cat("\n*** SENSITIVITY TO UNOBSERVED CONFOUNDERS ***\n")
  cat("\n### Worst Case Scenarios ###\n\n")
  worstcaseinterpret(x, q = q, scenarios=scenarios)
  cat("\n\n### Benchmarking ###\n")
  interpret(x, q = q)
}
