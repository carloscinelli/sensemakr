#' sensemakr: A package for sensitivity analysis, implementing "Making Sense of
#' Sensitivity: Extending Omitted Variable Bias"
#'
#' TODO: More documentation goes here
#'
#' @docType package
#' @name sensemakr-package
NULL



sensemakr.lm <- function(model,
                         treatment,
                         benchmark_covariates = NULL,
                         kd = 1,
                         ky = kd,
                         q = 1,
                         alpha = 0.05
                         ){



}

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

  if (is.null(treatment) || !treatment %in% all.vars(formula) ||
     (!is.null(data) && !treatment %in% colnames(data)) ||
     (is.null(data) && !exists(as.character(treatment)))
  ) {
    stop("You must provide a `treatment` variable present in the model ",
         "`formula` and `data` data frame.")
  }
  if (!is.null(data) && (!is.data.frame(data) || nrow(data) < 1)) {
    stop("The provided `data` argument must be a data frame with at least ",
         "one row.")
  }

  if (verbose) {
    print("Generating new formulae...")
  }

  treatment = as.character(treatment)
  formula_all = formula
  formula_treatment = update(formula,
                             paste(treatment, "~ . - ", treatment))
  if (verbose) {
    print("Outcome ~ Treatment formula:")
    print(formula_all)
    print("Treatment ~ Covariates formula:")
    print(formula_treatment)
    print("Fitting models...")
  }

  model_outcome = lm(formula_all, data = data)
  coef_outcome = coef(summary(model_outcome))
  model_treatment = lm(formula_treatment, data = data)

  if (verbose) {
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

  if (keep_model) {
    sensemakr[["model_outcome"]] = model_outcome
    sensemakr[["model_treatment"]] = model_treatment
  }

  sensemakr = structure(sensemakr, class = "sensemakr")

  if (!is.null(benchmark)) {
    if (verbose) {
      print("Adding benchmarks...")
    }

    sensemakr = add_benchmark(sensemakr, benchmark)
  }

  sensemakr
}



#' @rdname add_benchmark.sensemakr
#' @export
add_benchmark = function(obj, variables) {
  UseMethod("add_benchmark")
}

#' @rdname add_benchmark.sensemakr
#' @export
add_benchmark.default = function(obj, variables) {
  stop("`add_benchmark` is only defined for `sensemakr` objects.")
}

#' Add benchmarks to sensemakr object
#'
#' This function adds a benchmark variable to a `sensemakr` object. Benchmark
#' variables are variables which exist in the real models fit, but which stand
#' in for hypothetical unobserved variables and can attenuate observed effects
#' for variables of interest. Our suggestion is that subject-matter knowledge
#' guides the process of selecting a benchmark variable based on a belief that
#' it is the largest observed confounder.
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
#' # Adding a benchmark
#' sense.out = add_benchmark(sense.out, "age")
#'
#'
#' @param obj A `sensemakr` object to add benchmarks to.
#' @param variables A vector of character strings describing the variable or
#' variables to add as benchmarks
#' @return The `sensemakr` object `x` with benchmarks added.
#' @export
add_benchmark.sensemakr = function(obj, variables) {
  if (is.null(variables)) {
    stop("You must supply at least one additional benchmark variable to add ",
         "in an `add_benchmark` call.")
  }
  if (any(variables == obj[["treatment_variable"]])) {
    stop("A benchmark variable must not be the treatment variable, ",
         obj[["treatment_variable"]])
  }
  variables_in_model = variables %in% names(obj[["r2d"]]) &
    variables %in% names(obj[["r2y"]])

  if (!all(variables_in_model)) {
    missing_bench = variables[which(!variables_in_model)]
    stop("Benchmark variables specified must be present within the models fit ",
         "in the `sensemakr` object. The following benchmark variables are ",
         "invalid: ", paste(missing_bench, collapse = ", "))
  }

  benchmark_vars = lapply(variables, function(x) {
    bound = bound_calculator(
      obj[["r2y"]][x],
      obj[["r2d"]][x]
    )

    data.frame(variable = as.character(x),
               bound = unname(bias(
                 r2y = bound$r2_yz,
                 r2d = bound$r2_dz,
                 se = obj$treatment_effect[2],
                 dof = obj$dof
               )),
               r2y = unname(obj[["r2y"]][x]),
               r2d = unname(obj[["r2d"]][x]),
               stringsAsFactors = FALSE
    )
  })

  if (is.null(obj[["benchmark"]])) {
    obj[["benchmark"]] = do.call(rbind, benchmark_vars)
  } else {
    obj[["benchmark"]] = do.call(rbind,
                                 c(list(obj[["benchmark"]]), benchmark_vars))
  }

  structure(obj, class = "sensemakr")
}



#' @rdname remove_benchmarks.sensemakr
#' @export
remove_benchmarks = function(x) {
  UseMethod("remove_benchmarks")
}

#' @rdname remove_benchmarks.sensemakr
#' @export
remove_benchmarks.default = function(x) {
  stop("`remove_benchmarks` is only defined for `sensemakr` objects.")
}

#' Remove benchmarks from sensemakr object
#'
#' This function removes any benchmark variables attached to a `sensemakr`
#' object.
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
#' # Removing benchmarks
#' sense.out = remove_benchmarks(sense.out)
#'
#' @param x A `sensemakr` object to remove benchmarks from.
#' @return The `sensemakr` object `x` with benchmarks removed
#' @export
remove_benchmarks.sensemakr = function(x) {
  # Zap benchmarks
  ret = x[-which(names(x) %in%
                   c("benchmark", "benchmark_r2d", "benchmark_r2y"))]

  # Return as sensemakr object again
  structure(ret, class = "sensemakr")
}
