##' @title Senstivity abalysis of linear models
##'
##' @description Description.
##'
##' @param treatment  character vector with the treatment variable.
##' @param X  character vector with the covariates for benchmarking.
##' @param ... extra arguments
##' @return The function returns an object of class 'sensemakr' which is a list with the main
##' results for sensitivity analysis, namely:
##' \item{Treat Stats}{
##'    A list with the main statistics of the treatment estiamate which are needed for computing the sensitivity.
##'    \itemize{
##'       \item name
##'       \item estimate
##'       \item se
##'       \item df
##'       }
##'   }
##' \item{Benchmarks}{Benchmark values for the covariates listed on paramter X
##'     \itemize{
##'     \item R2 a data.frame with becnhmark values R2.
##'     \item SD a data.frame with benchmark values
##'     \item natural a data.frame with...
##'     }
##'   }
##'
##' @examples
# cleans workspace
##' rm(list = ls())
##'
##' # library
##' library(sensemakr)
##'
##' # loads data
##' data("darfur")
##'
##' # fits model
##' model  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
##'                pastvoted + hhsize_darfur + female + village, data = darfur)
##'
##' # benchmark variables
##' X = c("herder_dar", "female", "age", "pastvoted", "farmer_dar")
##'
##' # runs benchmarking etc
##' sense <- sensemakr(model, treatment = "directlyharmed", benchmarks = X)
##'
##' # plots
##'
##' ## contour plot
##' plot1_data <- plot(sense)
##' plot2_data <- plot(sense, contour = "t-value")
##' plot3_data <- plot(sense, contour = "lower bound")
##' plot4_data <- plot(sense, contour = "upper bound")
##'
##' ## worst-case plot
##' plot5_data <- plot(sense, type = "worst-case")
##'
##' # testing verbal outputs
##' interpret(sense)
##' interpret(sense, q = 0.5)
##'
##' summary(sense)
##'
##' @references
##' Cite paper(s)
##'
##' @export
##' @importFrom graphics abline legend lines plot points rug text
##' @importFrom stats coef df.residual formula model.matrix sd update vcov
sensemakr <- function(model, treatment, benchmarks, ...){
  UseMethod("sensemakr")
}


##' @param model the model.
##' @name sensemakr
##' @export
sensemakr.lm <- function(model, treatment, benchmarks=NULL){
  D <- treatment
  X <- benchmarks
  # stats <- get stats()
  # benchmarks <- get benchmarks()
  # compute bias and include ob data.frames
  # returns pretty list with class "sensemade"

  treat.stats <- getstats(model, D)
  benchmarks  <- benchmarkr(model, D, X)
  out <- list(treat.stats = treat.stats,
              benchmarks = benchmarks,
              info = list(outcome = deparse(model$terms[1][[2]]),
                          treatment = D,
                          model = model))
  class(out) <- "sensemade"
  return(out)
}


# Input: takes an lm object and the treatment name.
# It returns a data.frame with the names of the treatment, the estimate, the standard error,
# and the degrees of freedom.
#
getstats <- function(model, D){
  treat.summary     <- summary(model)
  df                <- treat.summary$df[2]
  estimate          <- coef(treat.summary)[D,"Estimate"]
  se                <- coef(treat.summary)[D,"Std. Error"]
  treat.stats       <- data.frame(treat = D,
                                  estimate = estimate,
                                  se = se,
                                  df = df,
                                  stringsAsFactors = FALSE,
                                  row.names = NULL)
  return(treat.stats)
}


# Input: takes an lm object + treatment name + covariates name
# covariates names -> if a single character vector, than each variable is a benchmark
#                  -> if a list of character vectors, then group by list (future work)
# Output: three data.frames with benchmarks for R2, SD and natural.
#       - data.frame contains: Names, R2y or delta, R2d or gamma
benchmarkr <- function(model, D, X = NULL, ...){

  treat.stats  <- getstats(model, D)
  estimate     <- treat.stats$estimate
  df.out       <- treat.stats$df
  sed          <- treat.stats$se
  summ.out     <- summary(model)
  coef.out     <- coef(summ.out)

  if (is.null(X)) {
    X <- rownames(coef.out)[!rownames(coef.out) %in% c("(Intercept)",D)]
  }

  tstats.out        <- coef.out[X, "t value"]
  r2y       <- tstats.out^2/(tstats.out^2 + df.out) # partial R2 with outcome


  string_formula_treat = noquote(paste0(D," ~ . ", paste0("-",D,collapse = "")))

  treat = update(model, string_formula_treat)

  # attr(terms(treat),'term.labels')  # no more backtick artifacts after update.lm()

  summ.treat     <- summary(treat)
  df.treat       <- summ.treat$df[2]

  ## R2d: gets t-statistics from treatment regression
  coef.treat <- coef(summ.treat)
  tstats.treat  <- coef.treat[X, "t value"] # excludes intercept
  r2d       <- tstats.treat^2/(tstats.treat^2 + df.treat) # partial R2 with treatment


  ## reverse engineering to get coefficients in original scale

  sed2      <- sed/(sqrt((1 - r2y)/(1 - r2d))*sqrt((df.out + 1)/(df.out))) # readjusts standard error
  covariate.bias      <- getbiasR2(r2y, r2d,se = sed2, df = df.out + 1)  # gets gamma*delta

  impact    <- coef(summ.out)[X, "Estimate"]
  imbalance <- covariate.bias/impact

  ## standardized coefficients
  sdy       <- sd(model$model[[1]]) # sd of outcome
  Xn        <- model.matrix(model)[,c(D,X)]
  sdd       <- sd(Xn[,D]) # sd of treatment
  sdx       <- apply(Xn[,X], 2, sd) # sd of covariates
  estimate_std <- estimate*(sdd/sdy)
  imp_std   <- impact*(sdx/sdy)
  imb_std   <- imbalance*(sdd/sdx)

  # space for groups R2
  # compute groups R2 and bind on r2y r2d
  # worst case scenario benchmark
  allvars <- rownames(coef.out)[!rownames(coef.out) %in% D]
  r2y_all <- groupR2(model, allvars)
  r2d_all <- groupR2(treat, allvars)
  bias_all <- getbiasR2(sed, df.out, r2y_all, r2d_all)

  # biases
  bias_r2 = getbiasR2(sed, df.out, r2y, r2d)
  bias_nat = impact*imbalance
  bias_std = imp_std*imb_std

  benchmark_all_vars <- data.frame(r2y_all = r2y_all,
                                  r2d_all = r2d_all,
                                  adj_est_all = adjust_estimate(estimate, bias_all),
                                  adj_se_r2 = getseR2(sed, df.out, r2y_all, r2d_all),
                                  adj_t_r2 = gettR2(estimate/sed, df.out, r2y_all, r2d_all),
                                  row.names = NULL,
                                  stringsAsFactors = FALSE)

  benchmark_R2  <- data.frame(covariate = X,
                              r2y = r2y,
                              r2d = r2d,
                              bias_r2 = bias_r2,
                              adj_est_r2 = adjust_estimate(estimate, bias_r2),
                              adj_se_r2 = getseR2(sed, df.out, r2y, r2d),
                              adj_t_r2 = gettR2(estimate/sed, df.out, r2y, r2d),
                              row.names = NULL,
                              stringsAsFactors = FALSE)
  benchmark_R2 <- benchmark_R2[order(benchmark_R2$bias_r2, decreasing = TRUE), ]

  benchmark_natural <- data.frame(covariate = X,
                                  impact = impact,
                                  imbalance = imbalance,
                                  bias_nat = bias_nat,
                                  adj_est_nat = adjust_estimate(estimate, bias_nat),
                                  row.names = NULL,
                                  stringsAsFactors = FALSE)
  benchmark_natural <- benchmark_natural[order(benchmark_natural$bias_nat, decreasing = TRUE), ]

  benchmark_std <- data.frame(covariate = X,
                              impact_std = imp_std,
                              imbalance_std = imb_std,
                              bias_std = bias_std,
                              adj_est_std = adjust_estimate(estimate_std, bias_std),
                              row.names = NULL,
                              stringsAsFactors = FALSE)
  benchmark_std <- benchmark_std[order(benchmark_std$bias_std, decreasing = TRUE), ]

  benchmarks <- list(benchmark_all_vars = benchmark_all_vars,
                     benchmark_R2 = benchmark_R2,
                     benchmark_natural = benchmark_natural,
                     benchmark_std = benchmark_std)

  return(benchmarks)

}


groupR2 <- function(model, coefs){

  # betas
  betas <- coef(model)

  # var-covar matrix
  V <- vcov(model)

  # degrees of freedom
  df <- df.residual(model)

  # coefs to group
  indx <- names(betas) %in% coefs

  if (length(indx) == 0) {
    warning(paste("No covariates found matching the word:", coefs, "\nNA returned"))
    return(NA)
  }

  # compute F and R2
  q <- length(indx)
  f <- (t(betas[indx]) %*% solve(V[indx, indx]) %*% betas[indx])/q
  f*q / (f*q + df)
}

##' @title Computes effects on estimate, standard error and t-value caused by unobserved confounder
##' @description  These functions compute the bias caused by an unobserved confounder with a specific pair
##' of partial R2 with the treatment and with the outcome.
##' with certain characteristics.
##'
##' @param se       standard error of original  treatment effect estimate
##' @param df       degrees of freedom of the original linear model
##' @param r2d      hypothetical partial R2 of the confounder with the treatment
##' @param r2y      hypothetical partial R2 of the confounder with the outcome
##' @param ...      extra arguments
##' @export
getbiasR2 <- function(se, df, r2d, r2y) {
  sqrt(r2y*r2d/(1 - r2d))*se*sqrt(df)
}

##' @export
##' @name getbiasR2
getseR2   <- function(se, df, r2y, r2d){
  sqrt((1 - r2y)/(1 - r2d))*se*sqrt(df/(df - 1))
}

##' @export
##' @name getbiasR2
gettR2    <- function(t,df, r2y, r2d){
  (t/sqrt(df) - sqrt(r2d*(r2y/(1 - r2y))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
}


adjust_estimate <- function(estimate, bias, reduce = TRUE){
  if (reduce) {
    return(sign(estimate)*(abs(estimate) - bias))
  } else {
    return(sign(estimate)*(abs(estimate) + bias))
  }
}
