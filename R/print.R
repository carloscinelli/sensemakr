#' Print a sensemakr object
#'
#' This function prints a sensemakr object, including all partial R^2 values for
#' covariates used to generate the sensemakr object, by default sorting by
#' variables in the order they were specified by the supplying formula.
#'
#' Additional printing parameters include `digits` (specifying how many digits
#' of precision to include in numeric tables), and `sort_by` (a character
#' string specifying how to sort covariates printed; valid values include
#' "default", "alpha", "R2D", and "R2Y").
#'
#' @param x A `sensemakr` object
#' @param print_covariates Logical, describing whether to print the covariates
#' used to produce the sensemakr object. Defaults to FALSE.
#' @param digits How many digits to round output to. Default is the greater of
#' 3 digits or getOption("digits") - 2.
#' @param sort_by A character string specifying how to sort the covariates, if
#' printed. Valid options are "default" ("model", which sorts by the order the
#' variables were specified in the formula generating the sensemakr object),
#' "model", "alpha" (alphabetical sort), "R2D" (sorts by R^2 with respect to
#' treatment, descending), and "R2Y" (sorts by R^2 with respect to outcome,
#' descending)
#' @param ... Further arguments passed to print, not currently used.
#'
#' @export
print.sensemakr = function(x,
                           print_covariates = FALSE,
                           sort_by = "default",
                           digits = max(3L, getOption("digits") - 2L),
                           ...
                           ) {
  cat("Generating Formula:\n", paste(deparse(x$formula_outcome),
                                       sep = "\n", collapse="\n"),
      "\n\n", sep = "")

  cat("Original Effect (", x$treatment_variable, "):\n ", sep ="")
  print(
    data.frame(
      Estimate = round(x$treatment_effect[1], digits),
      SE = round(x$treatment_effect[2], digits),
      DoF = x$dof
    ),
    row.names = FALSE)
  cat("\n")

  cat("Robustness Values:\n")
  print(
    c(R2_YD = round(unname(x$r2_yd), digits),
      RV = round(unname(x$rv), digits),
      `RV(t)` = round(unname(x$rv_t), digits))
  )
  cat("\n")

  if(!is.null(x$benchmark)) {
    cat("Benchmark Variable",
        ifelse(nrow(x$benchmark) > 1, "s", ""),
        ":\n", sep="")

    print(x$benchmark[, c("variable", "bound")],
          digits = digits)
    cat("\n")
  }

  if(print_covariates) {
    cat("Partial R^2 for all covariates:\n")
    # Get covariate R2D / R2Y
    cov_r2y = t(t(x$r2y))
    cov_r2d = t(t(x$r2d))
    # Remove treatment from R2Y so we can column merge
    cov_r2y = cov_r2y[-which(rownames(cov_r2y) == x$treatment_variable), ]
    cov_matrix = cbind(cov_r2y, cov_r2d)
    colnames(cov_matrix) = c("R2Y", "R2D")

    if(sort_by == "alpha") {
      cov_matrix = cov_matrix[order(rownames(cov_matrix)), ]
    } else if(sort_by == "R2Y") {
      cov_matrix = cov_matrix[order(cov_matrix[, "R2Y"], decreasing=TRUE), ]
    } else if(sort_by == "R2D") {
      cov_matrix = cov_matrix[order(cov_matrix[, "R2D"], decreasing=TRUE), ]
    }

    print(round(cov_matrix, digits=digits))
  }
  invisible(x)
}
