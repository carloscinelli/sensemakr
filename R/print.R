
# prints, summaries and reports -------------------------------------------


#' Sensitivity analysis print and summary methods for \code{sensemakr}
#'
#' @description
#' The \code{print} and \code{summary} methods provide verbal descriptions of the sensitivity analysis results
#' obtained with the function \code{\link{sensemakr}}. The function \code{\link{ovb_minimal_reporting}} provides the latex code for a minimal
#' sensitivity analysis reporting, as suggested in Cinelli and Hazlett (2018).
#'
#' @param ... arguments passed to other methods.
#' @param x an object of class \code{\link{sensemakr}}.
#' @param digits minimal number of \emph{significant} digits.
#'
#' @examples
#' # runs regression model
#' model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
#'                          pastvoted + hhsize_darfur + female + village,
#'                          data = darfur)
#'
#' # runs sensemakr for sensitivity analysis
#' sensitivity <- sensemakr(model, treatment = "directlyharmed",
#'                                benchmark_covariates = "female",
#'                                kd = 1:3)
#' # print
#' sensitivity
#'
#' # summary
#' summary(sensitivity)
#'
#' # prints latex code for minimal sensitivity analysis reporting
#' ovb_minimal_reporting(sensitivity)
#'
#' @references Cinelli, C. and Hazlett, C. "Making Sense of Sensitivity: Extending Omitted Variable Bias." (2018).
#'
#' @export
print.sensemakr = function(x,
                           digits = max(3L, getOption("digits") - 2L),
                           ...
                           ) {
  cat("Sensitivity Analysis to Unobserved Confounding\n\n")

  cat("Model Formula: ", paste(deparse(x$info$formula),
                                       sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  cat("Unadjusted Estimates of '", treatment <- x$sensitivity_stats$treatment, "':\n ")
  cat(" Coef. estimate:", estimate <- round(x$sensitivity_stats$estimate, digits), "\n ")
  cat(" Standard Error:", se <- round(x$sensitivity_stats$se, digits), "\n ")
  cat(" t-value:", t_statistic <- round(x$sensitivity_stats$t_statistic, digits), "\n")
  cat("\n")

  cat("Sensitivity Statistics:\n ")
  cat(" Partial R2 of treatment with outcome:", r2yd.x <- round(x$sensitivity_stats$r2yd.x,digits), "\n ")
  cat(" Robustness Value,", "q =", q <- x$info$q, ":", rv_q <- round(x$sensitivity_stats$rv_q,digits), "\n ")
  cat(" Robustness Value,", "q =", q, "alpha =", alpha <- x$info$alpha, ":", rv_qa <- round(x$sensitivity_stats$rv_qa,digits), "\n")
  cat("\n")
  cat("For more information, check summary.")


}

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


#' @rdname print.sensemakr
#' @param object an object of class \code{\link{sensemakr}}.
#' @export
summary.sensemakr <- function(object, digits = max(3L, getOption("digits") - 3L), ...){
  x <- object
  q <- x$info$q
  alpha <- x$info$alpha
  cat("Sensitivity Analysis to Unobserved Confounding\n\n")

  cat("Model Formula: ", paste(deparse(x$info$formula),
                               sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  cat("Unadjusted Estimates of", paste0("'",treatment <- x$sensitivity_stats$treatment,"':"), "\n ")
  cat(" Coef. estimate:", estimate <- round(x$sensitivity_stats$estimate, digits), "\n ")
  cat(" Standard Error:", se <- round(x$sensitivity_stats$se, digits), "\n ")
  cat(" t-value:", t_statistic <- round(x$sensitivity_stats$t_statistic, digits), "\n")
  cat("\n")

  cat("Sensitivity Statistics:\n ")
  cat(" Partial R2 of treatment with outcome:", r2yd.x <- round(x$sensitivity_stats$r2yd.x,digits), "\n ")
  cat(" Robustness Value,", "q =", paste0(q, ":"), rv_q <- round(x$sensitivity_stats$rv_q,digits), "\n ")
  cat(" Robustness Value,", "q =", paste0(q, ","), "alpha =", paste0(alpha, ":"), rv_qa <- round(x$sensitivity_stats$rv_qa,digits), "\n")
  cat("\n")
  reduce <- ifelse(x$info$reduce, "reduce", "increase")
  cat("Verbal interpretation of sensitivity statistics:\n\n")
  cat("Unobserved confounders (orthogonal to the covariates) that explain more than", 100*rv_q,"%", "of the residual variance",
      "of both the treatment and the outcome are enough to", reduce, "the absolute value of the effect size by", 100*q, "%.",
      "Conversely, unobserved confounders that do not explain more than", 100*rv_q,"%", "of the residual variance",
      "of both the treatment and the outcome are not strong enough to", reduce,"the absolute value of the effect size by",  100*q, "%.\n\n")

  cat("Unobserved confounders (orthogonal to the covariates) that explain more than", 100*rv_qa,"%", "of the residual variance",
      "of both the treatment and the outcome are enough to", reduce, "the absolute value of the effect size by", 100*q, "% at the significance level of alpha =", alpha, ".",
      "Conversely, unobserved confounders that do not explain more than", 100*rv_qa,"%", "of the residual variance",
      "of both the treatment and the outcome are not strong enough to", reduce,"the absolute value of the effect size by",  100*q, "% at the significance level of alpha =", alpha, ".\n\n")

  cat("An extreme confounder (orthogonal to the covariates) that explains 100% of the residual variance of the outcome, would need to explain at least",
      100*r2yd.x, "% of the residual variance of the treatment to fully account for the observed estimated effect.")
  cat("\n\n")

  bounds <- x$bounds
  if (!is.null(bounds)) {
    numeric <- sapply(bounds, is.numeric)
    bounds[numeric] <- lapply(bounds[numeric], round, digits = digits)
    names(bounds) <-  sapply(gsub("\\_", " ",  names(bounds)), .simpleCap)

    cat("OVB bounds:\n")
    print(bounds, row.names = FALSE)
  }
}


#' @rdname print.sensemakr
#' @export
ovb_minimal_reporting <- function(x, digits = 3, ...){

  # Let's begin
  table_settings = list(...)
  # Override variable labels
  outcome_label = ifelse(is.null(table_settings[["outcome_label"]]),
                         all.vars(x$info$formula)[1],
                         table_settings[["outcome_label"]])

  treatment_label = ifelse(is.null(table_settings[["treatment_label"]]),
                           x$sensitivity_stats$treatment,
                           table_settings["treatment_label"])



  # Okay, static beginning
  table_begin = paste0("\\begin{table}[!h]\n",
                       "\\centering\n",
                       "\\begin{tabular}{lrrrrrr}\n")

  # Outcome variable header
  outcome_header = sprintf("\\multicolumn{7}{c}{Outcome: \\textit{%s}} \\\\\n",
                           outcome_label)

  # Coeff header row
  coeff_header = paste0(
    "\\hline \\hline \n",
    "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ ",
    paste0("& $RV_{q = ", x$info$q, "}$ "),
    paste0("& $RV_{q = ", x$info$q, ", \\alpha = ", x$info$alpha, "}$ "),
    " \\\\ \n",
    "\\hline \n"
  )


  # Treatment result
  # Why paste and not sprintf? Easier to handle precision on the digits.
  coeff_results = paste0(
    "\\textit{", treatment_label, "} & ",
    round(x$sensitivity_stats$estimate, digits), " & ",
    round(x$sensitivity_stats$se, digits), " & ",
    round(x$sensitivity_stats$t_statistic, digits)," & ",
    100 * round(x$sensitivity_stats$r2yd.x, digits), "\\% & ",
    100 * round(x$sensitivity_stats$rv_q, digits), "\\% & ",
    100 * round(x$sensitivity_stats$rv_qa, digits), "\\% \\\\ \n")

  # Foonote row: Display benchmarks

  if (!is.null(x$bounds)) {
    footnote_begin = paste0("\\hline \n",
                            "df = ", x$sensitivity_stats$dof, " & & ",
                            "\\multicolumn{5}{r}{ ",
                            "")
    row = x$bounds[1, , drop = FALSE]

    footnotes = paste0("\\small ",
                       "\\textit{Bound (", row$bound_label, ")}: ",
                       "$R^2_{Y\\sim Z| {\\bf X}, D}$ = ",
                       100 * round(row$r2yz.dx, digits),
                       "\\%, $R^2_{D\\sim Z| {\\bf X} }$ = ",
                       100 * round(row$r2dz.x, digits),
                       "\\%")
    footnote_body = paste0(footnotes, collapse = " \\\\ ")
    footnote_end = "} \\\\\n"

    footnote = paste0(footnote_begin, footnote_body, footnote_end)
  } else {
    footnote = ""
  }

  # Below footnote end table, caption, label
  tabular_end = "\\end{tabular}\n"
  caption = ifelse(!is.null(table_settings[["caption"]]),
                   paste0("\\caption{", table_settings[["caption"]], "} \n"),
                   "")
  label = ifelse(!is.null(table_settings[["label"]]),
                 paste0("\\label{", table_settings[["label"]], "} \n"),
                 "")

  table_end = "\\end{table}"

  # Stick it all together
  table = paste0(table_begin, outcome_header, coeff_header,
                 coeff_results, footnote, tabular_end,
                 caption, label, table_end)

  # Cat to output valid LaTeX for rmarkdown
  cat(table)
}
