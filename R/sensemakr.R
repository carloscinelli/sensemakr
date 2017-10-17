##' @title The main function for senstivity analysis of linear models
##'
##' @description
##' This is the main function of the package. Users should run this function first to start their sensitivity analysis.
##'
##' @return The function returns an object of class 'sensemakr' which is a structured list containing
##' quantities for sensitivity analysis, namely:
##'
##' \item{info}{list with information of the original outcome model
##'    \itemize{
##'       \item model the \code{\link{lm}} object for the original outcome model
##'       \item outcome the name of the outcome
##'       \item treatment the name of the treatment
##'       }
##'   }
##' \item{treat.stats}{list with the main statistics of the original treatment estimate which are needed for computing the sensitivity analysis.
##'    \itemize{
##'       \item df the degrees of freedom
##'       \item estimate the estimate of the treatment effect
##'       \item se the standard error of the estimate
##'       \item treat the name of the treatment
##'       }
##'   }
##' \item{benchmarks}{list of data frames with sensitivity analysis benchmarks
##'     \itemize{
##'     \item benchmark_dropallvar contains R2 benchmark values for worst-case scenarios.
##'     \item benchmark_eachvar contains R2 benchmark values for all columns of the outcome model's design matrix.
##'     \item benchmark_group contains R2 benchmark values for groups.
##'     \item benchmark_masked contains non-redundent R2 benchmark values useful for summaries
##'     \item benchmark_natural contains natural-scaled benchmark values.
##'     }
##'   }
##'
##' @examples
##'
##' # cleans workspace
##' rm(list = ls())
##'
##' # library
##' library(sensemakr)
##'
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
##'
##' # draws plots
##' plot(sense)
##'
##' # text output
##' summary(sense)
##' summary(sense, q = 0.6)
##'
##'
##' @references
##' Cite paper(s)
##'
##' @param model the outcome model. Currently, only supports a formal \code{\link{lm}} object
##' @param treatment  a character string with the treatment variable's name
##' @param group_list a list of character vectors where elements within one vector are terms that should be grouped
##' @param ... extra arguments that might be passed to underlying functions
##'
##' @seealso The supported methods for a `sensemakr` object are:
##' \code{\link{plot.sensemakr}} \code{\link{print.sensemakr}} \code{\link{summary.sensemakr}}.
##' Also see \code{\link{lm}}.
##'
##' @export
##' @importFrom graphics abline legend lines plot points rug text
##' @importFrom stats coef df.residual formula model.matrix sd update vcov
sensemakr = function(model, treatment, group_list, ...){
  UseMethod("sensemakr")
}

##' @title  Obtains the robustness value at level q
##' @param R2_yd Partial R2 of the treatment explaining the outcome.
##' @param q the q value: we are concerned with bias of 100q% of the original estimate
##' @name get_RV
##' @export
get_RV = function(r2_yd, q=1){
  a=r2_yd/(1-r2_yd)
  return((q/2)*(sqrt(q^2*a^2+4*a)- q*a))
}


# note, name is sensemakr NOT sensemakr.lm, to link to single help doc
##' @name sensemakr
##' @export
sensemakr.lm = function(model, treatment, group_list=NULL,...){

  D = treatment  # notational convenience

  # stats = get stats()
  # benchmarks = get benchmarks()
  # compute bias and include ob data.frames
  # returns pretty list with class "sensemakr"

  treat.stats = getstats(model, D)

  t.treat = treat.stats$estimate/treat.stats$se

  r2_yd = t.treat^2/(t.treat^2+treat.stats$df)

  RV = get_RV(r2_yd=r2_yd, ...)

  benchmarks  = benchmarkr(model, D, # X,
                            group_list)
  # names(benchmarks)

  out = list(treat.stats = treat.stats,
              benchmarks = benchmarks,
             r2_yd = r2_yd,
             RV = RV,
              info = list(outcome = deparse(model$terms[1][[2]]),
                          treatment = D,
                          # maybe return 'treatment model'
                          model = model))
  class(out) = "sensemakr"
  return(out)
}


# Input: takes an lm object and the treatment name.
# Output: returns a data.frame with the names of the treatment, the estimate, the standard error,
# and the degrees of freedom.
# future plans, let user supply own output of getstats to be used in downstream benchmarkr()

# dont think this should be exported
getstats = function(model, D){
  treat.summary     = summary(model)
  df                = treat.summary$df[2]
  estimate          = coef(treat.summary)[D,"Estimate"]
  se                = coef(treat.summary)[D,"Std. Error"]
  treat.stats       = data.frame(treat = D,
                                  estimate = estimate,
                                  se = se,
                                  df = df,
                                  stringsAsFactors = FALSE,
                                  row.names = NULL)
  return(treat.stats)
}


# Input: takes an lm object + treatment name + covariates name
# Output: three data.frames with benchmarks for R2, SD and natural.
#       - data.frame contains: Names, R2y or delta, R2d or gamma

# dont think this should be exported
##' @importFrom stats terms
benchmarkr = function(model, D, # X = NULL,
                       group_list=NULL, ...){


  treat.stats  = getstats(model, D)
  estimate     = treat.stats$estimate
  df.out       = treat.stats$df
  sed          = treat.stats$se
  summ.out     = summary(model)
  coef.out     = coef(summ.out)


  # optional warning()
  # if "(Intercept)" not detected,
  # then ask user if NOT having an intercept in their outcome model was their intention
  # also disuade user NOT to manually create column of 1s as intercept
  # eg suggest to use formal lm(.~ 1) or lm(.~ -1)
  # as the name of the user created 1s column can vary wildly
  # internally, it is explicitly checking for the specific name value "(Intercept)" generated by lm(.~1)
  # lm(data=cbind(iris,custom_int=1),Sepal.Length~Sepal.Width+custom_int-1)
  # lm(data=cbind(iris,custom_int=1),Sepal.Length~Sepal.Width+1)

  if(!("(Intercept)" %in% rownames(coef.out))){
    warning("
            In your treatment model, we did not detect a formal '(Intercept)' column, a byproduct from 'lm(.~1)' .
            Was this your intention? Please include or exclude an intercept column using the formal linear model
            commands 'lm(.~1)' or 'lm(.~-1)' respectively. Do not rely on a manually created column of 1's")
  }

  # NOTE: figure out safer way to exclude Intercept not based on names
  X = rownames(coef.out)[!rownames(coef.out) %in% c("(Intercept)",D)]

  tstats.out        = coef.out[X, "t value"]
  r2y       = tstats.out^2/(tstats.out^2 + df.out) # partial R2 with outcome


  string_formula_treat = noquote(paste0(D," ~ . ", paste0("-",D,collapse = "")))

  treat = update(model, string_formula_treat)

  # attr(terms(treat),'term.labels')  # no more backtick artifacts after update.lm()

  summ.treat     = summary(treat)
  df.treat       = summ.treat$df[2]

  ## R2d: gets t-statistics from treatment regression
  coef.treat = coef(summ.treat)
  tstats.treat  = coef.treat[X, "t value"] # excludes intercept
  r2d       = tstats.treat^2/(tstats.treat^2 + df.treat) # partial R2 with treatment


  ## reverse engineering to get coefficients in original scale

  sed2      = sed/(sqrt((1 - r2y)/(1 - r2d))*sqrt((df.out + 1)/(df.out))) # readjusts standard error
  covariate.bias      = get_bias(r2y = r2y, r2d = r2d, se = sed2, df = df.out + 1)  # gets gamma*delta

  impact    = coef(summ.out)[X, "Estimate"]
  imbalance = covariate.bias/impact

  ## standardized coefficients
  sdy       = sd(model$model[[1]]) # sd of outcome
  Xn        = model.matrix(model)[,c(D,X)]
  sdd       = sd(Xn[,D]) # sd of treatment
  sdx       = apply(Xn[,X, drop = FALSE], 2, sd) # sd of covariates
  estimate_std = estimate*(sdd/sdy)
  imp_std   = impact*(sdx/sdy)
  imb_std   = imbalance*(sdd/sdx)


  all_rhs = (attr(terms(formula(model)),'term.labels'))
  allvars = all_rhs[!(all_rhs %in% D)]


  r2y_all = group_r2(model, allvars)
  r2d_all = group_r2(treat, allvars)
  bias_all = get_bias(se = sed, df = df.out, r2y = r2y_all, r2d = r2d_all)

  # biases
  bias_r2 = get_bias(se = sed, df = df.out, r2y = r2y, r2d =  r2d)
  bias_nat = impact*imbalance
  bias_std = imp_std*imb_std

  benchmark_dropallvar = data.frame(r2y_all = r2y_all,
                                   r2d_all = r2d_all,
                                   adj_est_all = adjust_estimate(estimate, bias_all),
                                   adj_se_r2 = get_se(se = sed, df = df.out, r2y = r2y_all,r2d =  r2d_all),
                                   adj_t_r2 = get_t(t = estimate/sed, df =  df.out, r2y = r2y_all, r2d = r2d_all),
                                   # row.names = NULL,
                                   stringsAsFactors = FALSE)


  benchmark_eachvar  = data.frame(# covariate = X,  # rely on row.names(benchmarks)
                              r2y = r2y,
                              r2d = r2d,
                              bias_r2 = bias_r2,
                              adj_est_r2 = adjust_estimate(estimate, bias_r2),
                              adj_se_r2 = get_se(se = sed, df = df.out, r2y = r2y, r2d = r2d),
                              adj_t_r2 = get_t(t = estimate/sed, df = df.out, r2y = r2y, r2d = r2d),
                              # row.names = NULL,
                              # use row.names later
                              stringsAsFactors = FALSE)

  benchmark_eachvar = benchmark_eachvar[order(benchmark_eachvar$bias_r2, decreasing = TRUE), ]

  benchmark_natural = data.frame(# covariate = X,  # rely on row.names(benchmarks)
                                  impact = impact,
                                  imbalance = imbalance,
                                  bias_nat = bias_nat,
                                  adj_est_nat = adjust_estimate(estimate, bias_nat),
                                  # row.names = NULL,
                                  stringsAsFactors = FALSE)

  benchmark_natural = benchmark_natural[order(benchmark_natural$bias_nat, decreasing = TRUE), ]

  benchmark_std = data.frame(# covariate = X,  # rely on row.names(benchmarks)
                              impact_std = imp_std,
                              imbalance_std = imb_std,
                              bias_std = bias_std,
                              adj_est_std = adjust_estimate(estimate_std, bias_std),
                              # row.names = NULL,
                              stringsAsFactors = FALSE)

  benchmark_std = benchmark_std[order(benchmark_std$bias_std, decreasing = TRUE), ]

  ##########################################
  # any 'blacklisted' terms (of outcome model)
  # that should be grouped?
  # usehelper function ?class_df_from_term
  ##########################################

  blacklist_4_group = c('factor','matrix','smooth')

  class_df = class_df_from_term(model)
  list_term_in_blacklist = lapply(X=class_df,FUN=function(XX){XX %in% blacklist_4_group})
  terms_in_blacklist = names(which(unlist(list_term_in_blacklist)))


  ############################################
  # ?group_r2
  # lapply cycle over 'group_list'
  # group_r2(model=model)
  # group_r2(model=treat)
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
                              group_r2(model=model,terms_4_group=XX)
                            })

    names(r2y_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(XX){
                                            paste0(paste(XX,collapse=","))
                                          }))


    r2d_combinevar = lapply(X=terms_force_group,
                            FUN=function(XX){
                              group_r2(model=treat,terms_4_group=XX)
                            })

    names(r2d_combinevar) = unlist(lapply(terms_force_group,
                                          FUN=function(XX){
                                            paste0(paste(XX,collapse=","))
                                          }))

    # bias from R2 param,
    # consult if this applies directly to group_r2
    r2pairs = cbind(r2y=r2y_combinevar,r2d=r2d_combinevar)

    bias_r2_combinevar = apply(X=r2pairs,MARGIN=1,
                               FUN=function(XX){
                                 get_bias(sed, df.out, r2y=XX$r2y, r2d=XX$r2d)
                                 })

    benchmark_group  = data.frame(# covariate = names(r2y_combinevar),  # rely on row.names(benchmarks)
                                      r2y = unlist(r2y_combinevar),
                                      r2d = unlist(r2d_combinevar),
                                      bias_r2 = bias_r2_combinevar,  # consult
                                      adj_est_r2 = adjust_estimate(estimate, bias_r2_combinevar),  # consult
                                      # row.names = NULL,
                                      stringsAsFactors = FALSE)

  }else{
    benchmark_group = NULL
  }

  #############################################
  # 'benchmark_masked' has redundant information
  # but needed for future optional masking of a
  # parent groups' lower level model matrix quantities
  # better to mask here in sensemakr()
  # and make use of it in later methods
  #############################################

  if(is.null(terms_force_group)==FALSE){

    # mask out redundant 'low-level' model matrix benchmarks
    # that are tied to their parent group

    names_mm_ea_group = lapply(X=terms_force_group,
                               FUN=function(terms_force_group){
                                 rhs_in_term = which((attr(terms(formula(model)),'term.labels')) %in% terms_force_group)
                                 indx_mm_of_term = (model$assign) %in% rhs_in_term
                                 # names_mm_of_term = colnames(X)[indx_mm_of_term]  # X was initial model.matrix

                                 names_mm_of_term = colnames(model.matrix(model))[indx_mm_of_term]
                                 return(names_mm_of_term)
                               })

    # rownames(benchmarks)
    # unique(unlist(names_mm_ea_group))
    # (!rownames(benchmarks) %in% unique(unlist(names_mm_ea_group)))
    # turn on row names
    benchmark_masked = benchmark_eachvar[(!rownames(benchmark_eachvar) %in% unique(unlist(names_mm_ea_group))), ]

    # NOTE: the rows in benchmark_masked are
    # 'design matrix benchmarks' (eg 'benchmark_eachvar')
    # that qualify to be masked

    # does not contain 'rows' in benchmark_group

    # eg later on in plot()
    # group to group masking not auto-supported by 'showvars'
    # design matrix to group masking is auto-supported by 'showvars'
    # see this edge case when

    # edgecase = sensemakr(group_list=list(c('village','female')))
    # plot(edgecase,showvars='masked')

    # 'village,female' is plotted
    # 'female' is not plotted, but 'village' (group) is plotted
    # NOTE: the factor levels of 'village' are not plotted

    # reason is, 'village' itself is a standalone group
    # hence present in (edgecase$benchmarks$benchmarks_group)

    # 'female' was in (edgecase$benchmarks$benchmark_eachvar)
    # whose elements qualify to be masked
    # notice, the factor levels of 'village' are masked
    # since they are also in (edgecase$benchmarks$benchmark_eachvar)


  }else{
    benchmark_masked = NULL
  }

  # comment out sd scale
  # benchmark_std = benchmark_std # not necessary
  #
  # benchmarks = list(benchmark_all_vars = benchmark_all_vars,  # used in worst case plot
  #                    # benchmark_R2 = benchmark_R2,
  #                    benchmark_masked = benchmark_masked,
  #                    benchmark_group = benchmark_R2_group,
  #                    benchmark_natural = benchmark_natural,
  #                    benchmark_std = benchmark_std)

  benchmarks = list(benchmark_dropallvar = benchmark_dropallvar,
                     benchmark_eachvar = benchmark_eachvar,
                     benchmark_group = benchmark_group,
                     benchmark_masked = benchmark_masked,
                     benchmark_natural = benchmark_natural)


  # names(benchmarks)
  # str(benchmarks,max.level = 1)

  return(benchmarks)

}


# dont think this should be exported

##' @title non-exported helper functions
##' @description  These helper functions compute low-level quantitites related to the bias caused
##' by an unobserved confounder with a specific pair
##' of partial R2 values with the treatment and with the outcome.
##' Sensitivity of: estimate, standard error, and t-value due by unobserved confounder
##'
##'
##' NOTE: These are low level helper functions that are NOT exported.
##'
##' @param se       standard error of original  treatment effect estimate
##' @param df       degrees of freedom of the original linear model
##' @param r2d      hypothetical partial R2 of the confounder with the treatment
##' @param r2y      hypothetical partial R2 of the confounder with the outcome
##'
##'
##'


get_bias = function(se, df, r2d, r2y) {
  sqrt(r2y*r2d/(1 - r2d))*se*sqrt(df)
}

# dont think this should be exported
# note, name is get_bias NOT get_se, to link to single help doc
##' @name get_bias
get_se = function(se, df, r2d, r2y){
  sqrt((1 - r2y)/(1 - r2d))*se*sqrt(df/(df - 1))
}


# dont think this should be exported
# note, name is get_bias NOT get_t, to link to single help doc
##' @name get_bias
##' @param t the t value of the treatment
##' @param reduce a logical (default TRUE) representing if the statistics should be reduced
get_t = function(t, df, r2d, r2y, reduce = TRUE){
  if (reduce) {
    adj_t = sign(t)*(abs(t)/sqrt(df) - sqrt(r2y*(r2d/(1 - r2d))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
  } else {
    adj_t = sign(t)*(abs(t)/sqrt(df) + sqrt(r2y*(r2d/(1 - r2d))))*sqrt((1 - r2d)/(1 - r2y))*sqrt(df - 1)
  }
  return(adj_t)
}

# dont think this should be exported
# note, name is get_bias NOT adjust_estimate, to link to single help doc
##' @name get_bias
##' @param estimate the treatment estimate
##' @param bias the bias amount
adjust_estimate = function(estimate, bias, reduce = TRUE){
  if (reduce) {
    return(sign(estimate)*(abs(estimate) - bias))
  } else {
    return(sign(estimate)*(abs(estimate) + bias))
  }
}

# dont think this should be exported
# note, name is get_bias NOT t_to_r2, to link to single help doc
##' @name get_bias
t_to_r2 = function(t, df){
  t^2/(t^2 + df)
}

# dont think this should be exported
# note, name is get_bias NOT group_r2, to link to single help doc
##' @name get_bias
##'
##' @param model an `lm` object
##' @param terms_4_group a character vector of one or more terms to be viewed together as a single group.
##' NOTE: The elements in 'terms_4_group' must match the character values of "(attr(terms(formula(model)),'term.labels'))".
##'
##'
##' @examples # none, these are low level helper functions that are not exposed to the user.
##' @importFrom stats terms formula update
group_r2 = function(model,terms_4_group){


  ## # @descriptionforms R2 quantities for a model.matrix column group tied to their model's term
  ## # @return a numeric scalar representing the R2 value of with-holding the model matrix columns associated with 'terms_4_group'

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
                'Please make Gre the character vectors in "terms_4_group" take on values from "attr(terms(model),"term.labels")".',
                'You may need to include backticks ``,as in "terms_4_group=list(c("`I(foo)`"))"'
    )
    stop(msg)
  }


  # betas
  betas = coef(model)

  # var-covar matrix
  V = vcov(model)

  # degrees of freedom
  df = df.residual(model)


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
  q = length(indx)

  f = (t(betas[indx]) %*% solve(V[indx, indx],betas[indx]))/q

  # mikenote: tuck two terms into solve(A,x) for num stabilit eg, solve(A,b) = inv(A) %*% b
  # f = (t(betas[indx]) %*% solve(V[indx, indx]) %*% betas[indx])/q

  # The r2 value to be returned
  r2_group = f*q / (f*q + df)
  return(r2_group)

  # mikenote: optional return q and f? if needed later in biasR2(r2y,r2d,k)
}


# mikenote: controlling what is in 'terms_4_group' allows us to enforce group_r2
# on blacklisted classes like 'factor'
# class_df = class_df_from_term(model)
# # class 'blacklist' check
# blacklist = c('factor','matrix','smooth')
# term_in_blacklist = lapply(X=class_df,FUN=function(x){x %in% blacklist})
# list_term_in_blacklist = names(which(unlist(term_in_blacklist)))
# r2_y_blacklist = sapply(X=list_term_in_blacklist,FUN=group_r2,model=model)
