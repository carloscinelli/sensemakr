
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



