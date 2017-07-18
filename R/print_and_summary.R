# prints ------------------------------------------------------------------

##' @export
print.sensemade <- function(x, str = TRUE, ...){

  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Outcome:", x$info$outcome, "\n\n")
  cat("Object content:\n")
  if (str) str(x, max.level = 1)
  cat("\n For more, use plot and summary.")
}


##' @export
summary.sensemade <- function(object, ...){
  # bunch of useful things
  # return list with several useful things
  # returns a obj of class summary.sensemade
  out <- object
  class(out) <- "summary.sensemade"
  out
}

##' @export
interpret <- function(sensemade, q = 1){
  idxr2y <- which.max(sensemade$benchmarks$benchmark_R2$r2y)
  idxr2d <- which.max(sensemade$benchmarks$benchmark_R2$r2d)
  estimate <- sensemade$treat.stats$estimate
  se <- sensemade$treat.stats$se
  t <- estimate/se
  df <- sensemade$treat.stats$df
  varR2D <- sensemade$benchmarks$benchmark_R2$covariate[idxr2d]
  maxR2d <- sensemade$benchmarks$benchmark_R2$r2d[idxr2d]
  varR2Y <- sensemade$benchmarks$benchmark_R2$covariate[idxr2y]
  maxR2y <- sensemade$benchmarks$benchmark_R2$r2y[idxr2d]
  r2dc   <- t^2/(t^2 + (maxR2y/q^2)*df)
  r2yc   <- ((q*t)^2)*((1 - maxR2d)/(maxR2d*df))

  cat("\n")
  cat("---Using the covariate most strongly associated with the treatment assignment as a benchmark---\n\n")
  if (r2yc > 1) {

    maxBias <- (getbiasR2(se, df = df, r2d = maxR2d, r2y = 1))

    cat("An unobserved confounder with the same partial R2 with the treatment as '",
        varR2D, "' (", round(maxR2d,3), ") ", " would be able to cause at most a bias of ",
        round(maxBias, 3)," with an adjusted treatment effect of ", round(adjust_estimate(estimate, maxBias), 3),
        " in the extreme case where the confouder explains all the residual variance of the outcome (R2y = 1).",
        sep = "")

  } else {

    cat("An unobserved confounder with the same partial R2 with the treatment as '",
        varR2D, "' (R2d = ", round(maxR2d,3), ") ", " would have to be at least ", round(r2yc/maxR2y, 1),
        " times as strongly associated with the outcome"," (R2y = ", round(r2yc,3), ") ",
        "in order to reduce the treatment effect in ", q*100, "%", sep = "")
  }
  cat("\n\n")
  cat("---Using the covariate most strongly associated with the outcome as a benchmark---\n\n")
  cat("An unobserved confounder with the same partial R2 with the outcome as '",
      varR2Y, "' (R2y = ", round(maxR2y,3), ") ", " would have to be at least ", round(r2dc/maxR2d, 1),
      " times as strongly associated with the treatment"," (R2d = ", round(r2dc, 3), ") ",
      "in order to reduce the treatment effect in ", q*100, "%", sep = "")
  cat("\n\n")
}


##' @export
print.summary.sensemade <- function(x, q = 1, ...){
 # pretty print for the summary
  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Outcome:", x$info$outcome, "\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Unadjusted Treatment Effect:", round(x$treat.stats$estimate, 3), "\n")
  cat("\n*** SENSITIVITY TO UNOBSERVED CONFOUNDERS ***\n")
  interpret(x, q = q)
}

# # this is the idea for the sensemade
# model <- lm(mtcars)
# model
# summarymodel <- summary(model)
# summarymodel
# stats:::summary.lm
# stats:::print.summary.lm


# bunch of useful functions for summary

