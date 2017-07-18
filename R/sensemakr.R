##' @title Senstivity abalysis of linear models
##'
##' @description Description.
##'
##' @param D  character vector with the treatment variable.
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
##' # worked out example
##'
##' @references
##' Cite paper(s)
##'
##' @export
sensemakr <- function(...){
  UseMethod("sensemakr")
}


##' @param model the model.
##' @name sensemakr
##' @export
sensemakr.lm <- function(model, D, group_list = NULL, ...){
  # stats <- get stats()
  # benchmarks <- get benchmarks()
  # compute bias and include ob data.frames
  # returns pretty list with class "sensemade"
  treat.stats <- getstats(model, D)
  benchmarks  <- benchmarkr(model, D, group_list)
  out <- list(treat.stats = treat.stats,
              benchmarks = benchmarks,
              model = model)
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
benchmarkr <- function(model, D, group_list = NULL, ...){

  treat.stats  <- getstats(model, D)
  estimate     <- treat.stats$estimate
  df.out       <- treat.stats$df
  sed          <- treat.stats$se
  summ.out     <- summary(model)
  coef.out     <- coef(summ.out)

  # if(is.null(X)){
    # coef.out=coef(summary(lm(data=iris,Sepal.Width~Sepal.Length)))
    # X is character vector of term names (non intercept, non treatment)
    X <- rownames(coef.out)[!(rownames(coef.out) %in% c("(Intercept)",D))]
    # carlos prototype X used as basic row name index of model matrix cols

    # the way X is used, I don't think we need X here
    # mike anticipated X to be user way to spec list of generic sets of terms
    # to be used as list of simultaneous vars
    # X = list('village',c('female','village'),'village')
  # }

  tstats.out        <- coef.out[X, "t value"]
  r2y       <- tstats.out^2/(tstats.out^2 + df.out) # partial R2 with outcome


  string_formula_treat = noquote(paste0(D," ~ . ", paste0("-",D,collapse = "")))

  treat = update(model, string_formula_treat)

  # attr(terms(treat),'term.labels')  # no more backtick artifacts after update.lm()

  summ.treat     <- summary(treat)
  df.treat       <- summ.treat$df[2]

  ## R2d: gets t-statistics from treatment regression
  coef.treat <- coef(summ.treat)
  pick       <- rownames(coef.treat)[!rownames(coef.treat) %in% "(Intercept)"]
  tstats.treat        <- coef.treat[pick, "t value"] # excludes intercept
  r2d       <- tstats.treat^2/(tstats.treat^2 + df.treat) # partial R2 with treatment


  ## reverse engineering to get coefficients in original scale

  sed2      <- sed/(sqrt((1 - r2y)/(1 - r2d))*sqrt((df.out + 1)/(df.out))) # readjusts standard error
  covariate.bias      <- getbiasR2(r2y, r2d,se = sed2, df = df.out + 1)  # gets gamma*delta

  impact    <- coef(summ.out)[X, "Estimate"]
  imbalance <- covariate.bias/impact

  ## standardized coefficients
  sdy       <- sd(model$model[[1]]) # sd of outcome
  Xn         <- model.matrix(model)
  sdd       <- sd(Xn[,2]) # sd of treatment
  sdx       <- apply(Xn[,-c(1:2)], 2, sd) # sd of covariates
  imp_std   <- impact*(sdx/sdy)
  imb_std   <- imbalance*(sdd/sdx)

  # space for groups R2
  # compute groups R2 and bind on r2y r2d

  # biases
  bias_r2 = getbiasR2(sed, df.out, r2y, r2d)
  bias_nat = impact*imbalance
  bias_std = imp_std*imb_std


  benchmark_R2  <- data.frame(covariate = X,
                              r2y = r2y,
                              r2d = r2d,
                              bias_r2 = bias_r2,
                              adj_est_r2 = adjust_estimate(estimate, bias_r2),
                              row.names = NULL,
                              stringsAsFactors = FALSE)

  benchmark_natural <- data.frame(covariate = X,
                                  impact = impact,
                                  imbalance = imbalance,
                                  bias_nat = bias_nat,
                                  adj_est_nat = adjust_estimate(estimate, bias_nat),
                                  row.names = NULL,
                                  stringsAsFactors = FALSE)

  benchmark_std <- data.frame(covariate = X,
                              impact_std = imp_std,
                              imbalance_std = imb_std,
                              bias_std = bias_std,
                              adj_est_std = adjust_estimate(estimate, bias_std),
                              row.names = NULL,
                              stringsAsFactors = FALSE)

  # any 'blacklisted' terms (outcome model) that should be grouped?
  # ?class_df_from_term
  class_df = class_df_from_term(model)
  blacklist_4_group = c('factor','matrix','smooth')
  list_term_in_blacklist = lapply(X=class_df,FUN=function(x){x %in% blacklist_4_group})
  terms_in_blacklist = names(which(unlist(list_term_in_blacklist)))


  ############################################
  # ?groupR2
  # lapply cycle over 'group_list'
  # groupR2(model=model)
  # groupR2(model=treat)
  ############################################
  # group_list = list('village',c('female','village'),'village')

  if(!is.null(group_list)){
    # user spec list of group terms + all RHS terms in group blacklist
    terms_force_group = append(group_list,terms_in_blacklist)
    # c(NULL,'a')
    # c(NULL,NULL)
  }else{
    terms_force_group = terms_in_blacklist
    # terms_force_group = NULL
  }

  if(!is.null(terms_force_group)){

    # terms_force_group = c('village','female')

    r2y_combinevar = lapply(X=terms_force_group,
                            FUN=function(X){
                              groupR2(model=model,terms_4_group=X)
                            })

    names(r2y_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(X){
                                            paste0(paste(X,collapse=","))
                                          }))


    r2d_combinevar = lapply(X=terms_force_group,
                            FUN=function(X){
                              groupR2(model=treat,terms_4_group=X)
                            })

    names(r2d_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(X){
                                            paste0(paste(X,collapse=","))
                                          }))

    # bias from R2 param,
    # consult if this applies to groupR2
    r2pairs = cbind(r2y=r2y_combinevar,r2d=r2d_combinevar)

    bias_r2_combinevar = apply(X=r2pairs,MARGIN=1,
                               FUN=function(X){
                                 getbiasR2(sed, df.out, r2y=X$r2y, r2d=X$r2d)
                                 })

    benchmark_R2_group  <- data.frame(covariate = names(r2y_combinevar),
                                      r2y = unlist(r2y_combinevar),
                                      r2d = unlist(r2d_combinevar),
                                      bias_r2 = bias_r2_combinevar,  # consult
                                      adj_est_r2 = adjust_estimate(estimate, bias_r2_combinevar),  # consult
                                      row.names = NULL,
                                      stringsAsFactors = FALSE)
  }else{
    benchmark_R2_group = NULL
  }


  benchmarks <- list(benchmark_R2 = benchmark_R2,
                     benchmark_R2_group = benchmark_R2_group,
                     benchmark_natural = benchmark_natural,
                     benchmark_std = benchmark_std)

  return(benchmarks)

}

##' @title Computes  bias caused by unobserved confounder
##' @description  This function computes the bias caused by an unobserved confounder
##' with certain characteristics.
##'
##' @param se       standard error of original  treatment effect estimate
##' @param df       degrees of freedom of the original linear model
##' @param r2d      hypothetical partial R2 of the confounder with the treatment
##' @param r2y      hypothetical partial R2 of the confounder with the outcome
getbiasR2 <- function(se, df, r2d, r2y, ...){
  bias <- sqrt(r2y*r2d/(1 - r2d))*se*sqrt(df)
  return(bias)
}

adjust_estimate <- function(estimate, bias){
  sign(estimate)*(abs(estimate) - bias)
  }


# getbiasR2.lm(model, D, r2d, r2y){
#
# }

# getbiasR2.sensemade()

# mike test push pull
