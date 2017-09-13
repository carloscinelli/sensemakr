# dont think this should be exported

#' @title helper function to figure out classes of a data.frame column used in lm
#' @description Looks up and returns the class of the data.frame column(s) associated with a model's term.
#'
#' @param model an 'lm' object
#' @param class_post_formula if FALSE (default), returns class of df col BEFORE any in-formula transformations applied to terms
#' if TRUE, returns class of df col AFTER any in-formula transformations applied to terms
#'
#' @return a named list containing named character vectors. The list names are model terms. The vector names are data frame column names.
#' The vector values are characters for the data frame column's class.
#' NOTE: If any in-formula transformations were applied, the data.frame referenced would represent an 'intermediate' one
#' that is not exactly the originating data.frame passed to the 'data' arg of lm(data=.).
#'
#' @examples
#' model = lm(data = iris,Sepal.Length ~ Species + as.numeric(Species) +
#'              Species:Sepal.Width + as.factor(Sepal.Width)+
#'              as.factor(Sepal.Width)*as.factor(Petal.Length)+
#'              as.numeric(Species)*as.factor(Petal.Length))
#'
#' class_df_from_term(model,class_post_formula = TRUE)
#'
#' class_df_from_term(model,class_post_formula = FALSE)
#' @export
##' @importFrom stats terms as.formula getCall
class_df_from_term = function(model,class_post_formula=FALSE){

  # reusable info
  hash_term = attr(terms(formula(model)),'factor')

  # iterate for each term in original parent formula
  # for each term, chop to individual formula involving only that term

  all_rhs_par_form = colnames(hash_term)

  list_term_form_uses_col_df = sapply(X=all_rhs_par_form,USE.NAMES=TRUE,
                                      FUN=function(one_term){
                                        return(all.vars(stats::as.formula(paste0('~',one_term))))
                                        # crucial that all.vars() interprets formula right
                                      })

  if(class_post_formula==TRUE){

    # class_post_formula=TRUE

    # return class of df col AFTER in-formula transformations applied to terms
    # note: informula '~ as.factor(x_cntns)' will return class 'factor'
    # looksup against 'attr(terms(model),'dataClasses')'


    list_class_df_from_term = lapply(names(list_term_form_uses_col_df),
                                     FUN=function(list_colnames_df_oneterm){

                                       # list_colnames_df_oneterm=names(list_term_form_uses_col_df)[5]
                                       classes_col_df = attr(terms(model),'dataClasses')[names(which(hash_term[,(list_colnames_df_oneterm)] != 0))]
                                       return(classes_col_df)
                                     })

    names(list_class_df_from_term) = names(list_term_form_uses_col_df)

  } else {

    # return class of df col BEFORE in-formula transformations applied to terms
    # note: informula '~ as.factor(x_cntns)' will return class of original 'x_cntns' and NOT 'factor'
    # looksup against 'dat_in = (eval(getCall(model)$data,environment(terms(model))))[1,]'

    # just one row
    dat_in = (eval(stats::getCall(model)$data,environment(terms(model))))[1,]

    list_class_df_from_term = lapply(list_term_form_uses_col_df,
                                     FUN=function(list_colnames_df_oneterm){
                                       # list_colnames_df_oneterm=list_term_form_uses_col_df[2]
                                       classes_col_df = sapply(dat_in[1,unlist(list_colnames_df_oneterm)],class,USE.NAMES = TRUE)
                                       names(classes_col_df) = as.character(unlist(list_colnames_df_oneterm,use.names=FALSE))

                                       return(classes_col_df)
                                     })

  }


  # NOTE: result is a string hence
  # is.factor(list_class_df_from_term[1]) will be false
  # since is.factor() checks class

  return(list_class_df_from_term)
}


# # df col classes pre 'in-formula' transforms
# class_df_from_term(model,class_post_formula = FALSE)
#
# # df col classes post 'in-formula' transforms
# class_df_from_term(model,class_post_formula = TRUE)
#
# str(iris)
#
# model  = lm(data = iris,Sepal.Length ~ Species + as.numeric(Species) +
#                Species:Sepal.Width + as.factor(Sepal.Width)+
#                as.factor(Sepal.Width)*as.factor(Petal.Length)+
#                as.numeric(Species)*as.factor(Petal.Length))
#
# terms(model)
#
# str(iris)
# made output list structure consistant
# model  = lm(data = iris,Sepal.Length ~ Species + Petal.Length)
# str(class_df_from_term(model,class_post_formula = FALSE))
# str(class_df_from_term(model,class_post_formula = TRUE))
