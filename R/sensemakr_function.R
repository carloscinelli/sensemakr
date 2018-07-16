#' Create a sensemakr object
#'
#' @param formula A formula describing the relationship Y ~ D + X
#' @param treatment A quoted character string naming the treatment variable in
#' `formula`.
#' @param data A data.frame object to fit models to.
#' @param benchmark A quoted character string. If provided, selects a
#' benchmark variable to compare potential unobserved confounders to.
#' @param verbose A logical value, if TRUE will print additional debugging
#' information while compiling the sensemakr object
#' @param keep_model A logical value, if TRUE will include `model_outcome` and
#' `model_treatment` models used to create `sensemakr` object.
#'
#' @examples
#' # Creating a sensemakr object using the built-in `darfur` data
#' data(darfur)
#' sense.out = sensemakr(formula = peacefactor ~ directlyharmed + female +
#'                         village + age,
#'                       data = darfur,
#'                       treatment = "directlyharmed",
#'                       benchmark = "female")
#'
#' @return A `sensemakr` object.
#' @importFrom stats lm update
#' @export
sensemakr = function(formula,
                     treatment,
                     data = NULL,
                     benchmark = NULL,
                     verbose = FALSE,
                     keep_model = FALSE) {

  if(length(all.vars(formula)) < 3) {
    stop("The `formula` argument must contain a dependent variable and at ",
         "least two predictors")
  }
  if(is.null(treatment) || !treatment %in% all.vars(formula) ||
     (!is.null(data) && !treatment %in% colnames(data)) ||
     (is.null(data) && !exists(as.character(treatment)))
     ) {
    stop("You must provide a `treatment` variable present in the model ",
         "`formula` and `data` data frame.")
  }
  if(!is.null(data) && (!is.data.frame(data) || nrow(data) < 1)) {
    stop("The provided `data` argument must be a data frame with at least ",
         "one row.")
  }

  if(verbose) {
    print("Generating new formulae...")
  }
  treatment = as.character(treatment)
  formula_all = formula
  formula_treatment = update(formula,
                             paste(treatment, "~ . - ", treatment))
  if(verbose) {
    print("Outcome ~ Treatment formula:")
    print(formula_all)
    print("Treatment ~ Covariates formula:")
    print(formula_treatment)
    print("Fitting models...")
  }

  model_outcome = lm(formula_all, data = data)
  coef_outcome = coef(summary(model_outcome))
  model_treatment = lm(formula_treatment, data = data)

  if(verbose) {
    print("Compiling results...")
  }

  sensemakr = list(
    formula_outcome = formula_all,
    formula_treatment = formula_treatment,
    treatment_variable = treatment,
    r2y = partial_r2(model_outcome),
    r2d = partial_r2(model_treatment),
    t = coef_outcome[, "t value"],
    rv = robustness_value(model_outcome, covariate = treatment),
    rv_t = robustness_value(model_outcome, covariate = treatment, alpha = 0.05),
    r2_yd = partial_r2(model_outcome)[treatment],
    treatment_effect = c(coef_outcome[treatment, "Estimate"],
                         coef_outcome[treatment, "Std. Error"]),
    dof = model_outcome$df.residual)

  if(keep_model) {
    sensemakr[["model_outcome"]] = model_outcome
    sensemakr[["model_treatment"]] = model_treatment
  }

  sensemakr = structure(sensemakr, class = "sensemakr")

  if(!is.null(benchmark)) {
    if(verbose) {
      print("Adding benchmarks...")
    }

    sensemakr = add_benchmark(sensemakr, benchmark)
  }

  sensemakr
}
