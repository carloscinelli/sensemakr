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
#' @param obj A `sensemakr` object to add benchmarks to.
#' @param variables A vector of character strings describing the variable or
#' variables to add as benchmarks
#' @return The `sensemakr` object `x` with benchmarks added.
#' @export
add_benchmark.sensemakr = function(obj, variables) {
  if(is.null(variables)) {
    stop("You must supply at least one additional benchmark variable to add ",
         "in an `add_benchmark` call.")
  }
  if(any(variables == obj[["treatment_variable"]])) {
    stop("A benchmark variable must not be the treatment variable, ",
         obj[["treatment_variable"]])
  }
  variables_in_model = variables %in% names(obj[["r2d"]]) &
    variables %in% names(obj[["r2y"]])

  if(!all(variables_in_model)) {
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
               bound = unname(bias_in_r2(
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

  if(is.null(obj[["benchmark"]])) {
    obj[["benchmark"]] = do.call(rbind, benchmark_vars)
  } else {
    obj[["benchmark"]] = do.call(rbind,
                                 c(list(obj[["benchmark"]]), benchmark_vars))
  }

  structure(obj, class = "sensemakr")
}
