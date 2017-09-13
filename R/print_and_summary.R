
# A method must have all the arguments of the generic, including … if the generic does.
# A method must have arguments in exactly the same order as the generic.

##' @title The print method for a sensemakr object
##' @description provides text interpretation in the console
##' @param x a `sensemakr` object, result of \code{\link{sensemakr}}
##' @param str a logical (default TRUE) indicating if the structure should be printed.
##' @param ... extra arguments that might be passed to underlying functions
##'
##' @seealso \code{\link{print}}
##'
##' @examples
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # runs benchmarking etc
##' sense = sensemakr(model, treatment = "directlyharmed")
##'
##' print(sense)
##' print(sense,str=FALSE)
##'
##' @export
print.sensemakr = function(x,str = TRUE,...){
  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Outcome:", x$info$outcome, "\n\n")
  cat("Object content:\n")
  if (str) str(x, max.level = 1)
  cat("\n For more, use plot and summary.")
}

##' @title The summary method for a sensemakr object
##' @description creates summary text interpretation
##' @param object a 'sensemakr' object, result of \code{\link{sensemakr}}
##' @param ... extra arguments that might be passed to underlying functions
##' @examples
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # runs benchmarking etc
##' sense = sensemakr(model, treatment = "directlyharmed")
##'
##' summary(sense)
##'
##' out_summary = summary(sense)
##' class(out_summary)
##'
##' @export
summary.sensemakr = function(object,...){

  # bunch of useful things
  # return list with several useful things
  # returns a obj of class summary.sensemakr

  # print.summary.sensemakr(out, q=q, scenarios=scenarios)

  # if we use the print.summary.sensemakr() here internally,
  # the class(out) is not assigned if out_external = summary.sensemakr(object)
  # class(out_external) is NULL
  # further, since summary.sensemakr(...) has elipses, can just pass them onto
  # print.summary.sensemakr(), further print.summary.sensemakr already
  # has these default arg values,
  # so redundant to assign them here in summary.sensemakr()

  out = object
  class(out) = "summary.sensemakr"
  return(out)

}


##' @title a function to interpret sensemakr quantities
##' @name interpret
##' @description provides text interpretation in the console
##' @param sensemakr a 'sensemakr' object, result of \code{\link{sensemakr}}
##' @param q numeric value between 0 and 1 representing proportion of alteration to treatment estimates
##' @seealso \code{\link{summary.sensemakr}} \code{\link{sensemakr}}
##' @examples
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # runs benchmarking etc
##' sense = sensemakr(model, treatment = "directlyharmed")
##'
##' interpret(sense, q = 0.5)
##' interpret(sense, q = 0.6)
##' @export
interpret = function(sensemakr, q = 1){

  # from: sensemakr$benchmarks$benchmark_eachvar$r2y
  # to: sensemakr$benchmarks$benchmark_eachvar$r2y

  # make sure 'max()' is choosing right benchmark point
  # currently points at 'female' not 'village',
  # since 'village' is in (sense$benchmarks$benchmark_group)
  # currently, only looking at (sensemakr$benchmarks$benchmark_eachvar)

  idxr2y = which.max(sensemakr$benchmarks$benchmark_eachvar$r2y)
  idxr2d = which.max(sensemakr$benchmarks$benchmark_eachvar$r2d)
  estimate = sensemakr$treat.stats$estimate
  se = sensemakr$treat.stats$se
  t = estimate/se
  df = sensemakr$treat.stats$df

  # deprecating benchmarks$covariate
  # rely on row.names(benchmarks)
  # varR2D = sensemakr$benchmarks$benchmark_eachvar$covariate[idxr2d]
  varR2D = row.names(sensemakr$benchmarks$benchmark_eachvar)[idxr2d]

  maxR2d = sensemakr$benchmarks$benchmark_eachvar$r2d[idxr2d]

  # deprecating benchmarks$covariate
  # rely on row.names(benchmarks)
  # varR2Y = sensemakr$benchmarks$benchmark_eachvar$covariate[idxr2y]
  varR2Y = row.names(sensemakr$benchmarks$benchmark_eachvar)[idxr2y]


  maxR2y = sensemakr$benchmarks$benchmark_eachvar$r2y[idxr2d]
  r2dc   = t^2/(t^2 + (maxR2y/q^2)*df)
  r2yc   = ((q*t)^2)*((1 - maxR2d)/(maxR2d*df))

  cat("\n")
  cat("---Using the covariate most strongly associated with the treatment assignment as a benchmark---\n\n")
  if (r2yc > 1) {

    maxBias = (get_bias(se, df = df, r2d = maxR2d, r2y = 1))

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

##' @title a function to interpret sensemakr quantities
##' @name worstcaseinterpret
##' @description provides text interpretation in the console
##' @param sensemakr a 'sensemakr' object, result of \code{\link{sensemakr}}
##' @param scenarios a numeric vector where each element represents a R2 scenario
##' @param q a numeric value between 0 and 1 representing proportion of alteration to treatment estimates
##' @seealso \code{\link{summary.sensemakr}}
##' @examples
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # runs benchmarking etc
##' sense = sensemakr(model, treatment = "directlyharmed")
##'
##' worstcaseinterpret(sense)
##'
##' @export
worstcaseinterpret = function(sensemakr, scenarios = c(1, 0.25), q = 1){
  estimate = sensemakr$treat.stats$estimate
  se = sensemakr$treat.stats$se
  t = estimate/se
  df = sensemakr$treat.stats$df
  r2dc   = t^2/(t^2 + (scenarios/q^2)*df)

  cat("Considering the extreme scenarios of unobserved confounders explaining ",
  paste0(scenarios*100, "%", collapse = ", "), " of the residual variance of the outcome, they would have",
  " to, respectively, explain at least ", paste0(round(r2dc*100, 2), "%", collapse = ", "),
  " of the treatment assignment to reduce the treatment effect in ", round(q*100, 2), "%.", sep = "")
}

# do not export print.summary.sensemakr
# to mimic doc of ?print.summary.lm
# to truely mimic
# looks like 'summary.sensemakr needs to be itself a standalone class
# trick is to just use 'summary.sensemakr' as the @name for print.summary.sensemakr

##' @name summary.sensemakr
##' @param x a `summary.sensemakr` object, result of \code{\link{summary.sensemakr}}
##' @param q numeric value between 0 and 1 representing proportion of alteration to treatment estimates
##' @param scenarios a numeric vector where each element represents a R2 scenario
##' @param ... extra arguments that might be passed to underlying functions
##'
##' @seealso \code{\link{print}} \code{\link{sensemakr}}
##'
##' @examples
##'
##' print(summary(sense))
##'
##' # NOTE: it is ?print.summary.sensemakr that controls different scenarios
##' # not ?summary.sensemakrhttp://lexis.ccpr.ucla.edu:8787/help/library/sensemakr/html/summary.sensemakr.html
##'
##' print(summary(sense),q = 0.2, scenarios = 0.5)
##'
print.summary.sensemakr = function(x, q = 1, scenarios = c(1,.25),...){
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
