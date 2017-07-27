##' @title Senstivity abalysis of linear models
##'
##' @description Description.
##'
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
##' interpret(sense, q = 0.6)
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
##' @param treatment  character vector with the treatment variable.
##' @param benchmarks  character vector with the covariates for benchmarking.
##' @param group_list a list of character vectors where elements within one vector are terms that should be grouped.
##' @name sensemakr
##' @export
sensemakr.lm <- function(model, treatment, benchmarks=NULL, group_list=NULL){
  D <- treatment
  X <- benchmarks
  # stats <- get stats()
  # benchmarks <- get benchmarks()
  # compute bias and include ob data.frames
  # returns pretty list with class "sensemade"

  treat.stats <- getstats(model, D)
  benchmarks  <- benchmarkr(model, D, X, group_list)
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
benchmarkr <- function(model, D, X = NULL, group_list=NULL, ...){

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
  covariate.bias      <- get_bias(r2y = r2y, r2d = r2d, se = sed2, df = df.out + 1)  # gets gamma*delta

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
  # allvars <- rownames(coef.out)[!rownames(coef.out) %in% D]

  # rownames of coef printout is not safe
  # mike note: use this instead
  all_rhs = (attr(terms(formula(model)),'term.labels'))
  allvars = all_rhs[!(all_rhs %in% D)]

  # 1 conflict resolved here, when merging group main branch
  # =======
  # allvars <- rownames(coef.out)[!rownames(coef.out) %in% c(D,"(Intercept)")]
  # >>>>>>> master

  r2y_all <- groupR2(model, allvars)
  r2d_all <- groupR2(treat, allvars)
  bias_all <- get_bias(se = sed, df = df.out, r2y = r2y_all, r2d = r2d_all)

  # biases
  bias_r2 = get_bias(se = sed, df = df.out, r2y = r2y, r2d =  r2d)
  bias_nat = impact*imbalance
  bias_std = imp_std*imb_std

  benchmark_all_vars <- data.frame(r2y_all = r2y_all,
                                  r2d_all = r2d_all,
                                  adj_est_all = adjust_estimate(estimate, bias_all),
                                  adj_se_r2 = get_se(se = sed, df = df.out, r2y = r2y_all,r2d =  r2d_all),
                                  adj_t_r2 = get_t(t = estimate/sed, df =  df.out, r2y = r2y_all, r2d = r2d_all),
                                  row.names = NULL,
                                  stringsAsFactors = FALSE)

  benchmark_R2  <- data.frame(covariate = X,
                              r2y = r2y,
                              r2d = r2d,
                              bias_r2 = bias_r2,
                              adj_est_r2 = adjust_estimate(estimate, bias_r2),
                              adj_se_r2 = get_se(se = sed, df = df.out, r2y = r2y, r2d = r2d),
                              adj_t_r2 = get_t(t = estimate/sed, df = df.out, r2y = r2y, r2d = r2d),
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


  # any 'blacklisted' terms (outcome model) that should be grouped?
  # ?class_df_from_term
  class_df = class_df_from_term(model)
  blacklist_4_group = c('factor','matrix','smooth')
  list_term_in_blacklist = lapply(X=class_df,FUN=function(XX){XX %in% blacklist_4_group})
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
                            FUN=function(XX){
                              groupR2(model=model,terms_4_group=XX)
                            })

    names(r2y_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(XX){
                                            paste0(paste(XX,collapse=","))
                                          }))


    r2d_combinevar = lapply(X=terms_force_group,
                            FUN=function(XX){
                              groupR2(model=treat,terms_4_group=XX)
                            })

    names(r2d_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(XX){
                                            paste0(paste(XX,collapse=","))
                                          }))

    # bias from R2 param,
    # consult if this applies to groupR2
    r2pairs = cbind(r2y=r2y_combinevar,r2d=r2d_combinevar)

    bias_r2_combinevar = apply(X=r2pairs,MARGIN=1,
                               FUN=function(XX){
                                 get_bias(sed, df.out, r2y=XX$r2y, r2d=XX$r2d)
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



  benchmarks <- list(benchmark_all_vars = benchmark_all_vars,
                     benchmark_R2 = benchmark_R2,
                     benchmark_R2_group = benchmark_R2_group,
                     benchmark_natural = benchmark_natural,
                     benchmark_std = benchmark_std)

  return(benchmarks)

}


#' @title The function '?groupR2()'
#' @description forms R2 quantities for a model.matrix column group tied to their model's term
#'
#' @param model an 'lm' object
#' @param terms_4_group a character vector of one or more terms to be viewed together as a single group.
#' NOTE: The elements in 'terms_4_group' must match the character values of "(attr(terms(formula(model)),'term.labels'))".
#'
#' @return a numeric scalar representing the R2 value of with-holding the model matrix columns associated with 'terms_4_group'
#' @export
#'
#' @examples none, a low level helper

groupR2 <- function(model,terms_4_group){

  # carlos example
  # allvars <- rownames(coef.out)[!rownames(coef.out) %in% D]
  # terms_4_group = allvars


  # use terms instead
  # allvars = (attr(terms(formula(model)),'term.labels'))
  # terms_4_group = allvars

  # using the auto printout of coef reduces paste(term,level)
  # 'villageZalingi'
  # thats why use term(formula) so can just use 'village'


  #######################################
  # arg check
  # terms_4_group = c('village','blah')
  # terms_4_group = list('female',
  #                      c('village','female'),
  #                      'blah',
  #                      c('village',"`I(age/5)`"))

  #######################################

  lgl_a_term_not_found = lapply(X=terms_4_group,
                                FUN=function(X){
                                  # X=terms_4_group[[1]]
                                  # note: wants backticked "`I(age/5)`"
                                  lgl_a_term_not_found = any(!(unlist(X) %in% ((attr(terms(formula(model)),'term.labels')))))
                                })

  if(any(unlist(lgl_a_term_not_found))==TRUE){

    msg = paste('One of your terms specified in the "terms_4_group" argument is not found in your "model" argument.',
                'Please make sure the character vectors in "terms_4_group" take on values from "attr(terms(model),"term.labels")".',
                'You may need to include backticks ``,as in "terms_4_group=list(c("`I(foo)`"))"'
    )
    stop(msg)
  }


  # betas
  betas <- coef(model)

  # var-covar matrix
  V <- vcov(model)

  # degrees of freedom
  df <- df.residual(model)


  # (attr(terms(formula(model)),'term.labels'))
  # terms_4_group = 'village'  # set of factor levels as a group
  # terms_4_group = c('village','female')  # set of covars as a group

  rhs_in_term = which((attr(terms(formula(model)),'term.labels')) %in% terms_4_group)

  # use model$assign instead of attr(model.matrix(model),'assign')
  # identical(model$assign,attr(model.matrix(model),'assign'))
  # indx_mm_of_term = (attr(model.matrix(model),'assign')) %in% rhs_in_term

  indx_mm_of_term = (model$assign) %in% rhs_in_term


  indx = which(indx_mm_of_term)

  if (length(indx) == 0) {

    warning(paste("No model.matrix columns found matching the term:",
                  terms_4_group,
                  "\nNA returned"))

    return(NA)

  }


  # compute F and R2
  q <- length(indx)

  f <- (t(betas[indx]) %*% solve(V[indx, indx],betas[indx]))/q

  # mikenote: tuck two terms into solve(A,x) for num stabilit eg, solve(A,b) = inv(A) %*% b
  # f <- (t(betas[indx]) %*% solve(V[indx, indx]) %*% betas[indx])/q

  # The r2 value to be returned
  r2_group = f*q / (f*q + df)
  return(r2_group)

  # mikenote: optional return q and f? if needed later in biasR2(r2y,r2d,k)
}

# mikenote: controlling 'terms_4_group' allows us to enforce groupR2
# on blacklisted classes like 'factor'
# class_df = class_df_from_term(model)
# # class 'blacklist' check
# blacklist = c('factor','matrix','smooth')
# term_in_blacklist = lapply(X=class_df,FUN=function(x){x %in% blacklist})
# list_term_in_blacklist = names(which(unlist(term_in_blacklist)))
# r2_y_blacklist = sapply(X=list_term_in_blacklist,FUN=groupR2,model=model)



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
get_bias <- function(se, df, r2y, r2d) {
  sqrt(r2y*r2d/(1 - r2d))*se*sqrt(df)
}

##' @export
##' @name get_bias
get_se   <- function(se, df, r2y, r2d){
  sqrt((1 - r2y)/(1 - r2d))*se*sqrt(df/(df - 1))
}

##' @export
##' @name get_bias
get_t    <- function(t, df, r2y, r2d, reduce = TRUE){
  if (reduce) {
   adj_t <- sign(t)*(abs(t)/sqrt(df) - sqrt(r2y*(r2d/(1 - r2d))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
  } else {
    adj_t <- sign(t)*(abs(t)/sqrt(df) + sqrt(r2y*(r2d/(1 - r2d))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
  }
  return(adj_t)

}

# get_t2    <- function(r2, df, r2y, r2d){
#   (sqrt((r2)/(1-r2)) - sqrt(r2y*(r2d/(1 - r2d))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
# }



adjust_estimate <- function(estimate, bias, reduce = TRUE){
  if (reduce) {
    return(sign(estimate)*(abs(estimate) - bias))
  } else {
    return(sign(estimate)*(abs(estimate) + bias))
  }
}

t_to_r2 <- function(t, df){
  t^2/(t^2 + df)
}
