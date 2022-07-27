
# prints, summaries and reports -------------------------------------------


#' Sensitivity analysis print and summary methods for \code{sensemakr}
#'
#' @description
#' The \code{print} and \code{summary} methods provide verbal descriptions of the sensitivity analysis results
#' obtained with the function \code{\link{sensemakr}}. The function \code{\link{ovb_minimal_reporting}} provides
#' latex or html code for a minimal
#' sensitivity analysis reporting, as suggested in Cinelli and Hazlett (2020).
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
#' @references Cinelli, C. and Hazlett, C. (2020), "Making Sense of Sensitivity: Extending Omitted Variable Bias." Journal of the Royal Statistical Society, Series B (Statistical Methodology).
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

  reduce <- x$info$reduce
  q <- x$info$q

  cat("Null hypothesis:", "q =", q, "and", "reduce =", reduce,"\n\n")

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
  reduce <- x$info$reduce
  h0 <- round(ifelse(reduce, x$sensitivity_stats$estimate*(1 - q), x$sensitivity_stats$estimate*(1 + q)), 4)
  direction <- ifelse(reduce, "reduce", "increase")
  cat("Sensitivity Analysis to Unobserved Confounding\n\n")

  cat("Model Formula: ", paste(deparse(x$info$formula),
                               sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("Null hypothesis:", "q =", q, "and", "reduce =", reduce,"\n")
  cat("-- This means we are considering biases that", direction, "the absolute value of the current estimate.\n")
  cat("-- The null hypothesis deemed problematic is H0:tau =", h0, "\n")
  cat("\n")

  cat("Unadjusted Estimates of", paste0("'",treatment <- x$sensitivity_stats$treatment,"':"), "\n ")
  cat(" Coef. estimate:", estimate <- round(x$sensitivity_stats$estimate, digits), "\n ")
  cat(" Standard Error:", se <- round(x$sensitivity_stats$se, digits), "\n ")
  cat(" t-value", "(H0:tau =", paste0(h0,"):"), t_statistic <- round(x$sensitivity_stats$t_statistic, digits), "\n")
  cat("\n")

  cat("Sensitivity Statistics:\n ")
  cat(" Partial R2 of treatment with outcome:", r2yd.x <- round(x$sensitivity_stats$r2yd.x,digits), "\n ")
  cat(" Robustness Value,", "q =", paste0(q, ":"), rv_q <- round(x$sensitivity_stats$rv_q,digits), "\n ")
  cat(" Robustness Value,", "q =", paste0(q, ","), "alpha =", paste0(alpha, ":"), rv_qa <- round(x$sensitivity_stats$rv_qa,digits), "\n")
  cat("\n")
  reduce <- ifelse(x$info$reduce, "reduce", "increase")

  cat("Verbal interpretation of sensitivity statistics:\n\n")
  cat("-- Partial R2 of the treatment with the outcome: an extreme confounder (orthogonal to the covariates) that explains 100% of the residual variance of the outcome, would need to explain at least",
      paste0(100*r2yd.x, "%"),"of the residual variance of the treatment to fully account for the observed estimated effect.")
  cat("\n\n")
  cat("-- Robustness Value,", "q =", paste0(q, ":"), "unobserved confounders (orthogonal to the covariates) that explain more than",
      paste0(100*rv_q,"%"), "of the residual variance",
      "of both the treatment and the outcome are strong enough to bring the point estimate to", paste0(h0),
      "(a bias of", paste0(100*q, "%"),"of the original estimate).",
      "Conversely, unobserved confounders that do not explain more than", paste0(100*rv_q,"%"), "of the residual variance",
      "of both the treatment and the outcome are not strong enough to bring the point estimate to", paste0(h0,"."))
  cat("\n\n")
  cat("-- Robustness Value,", "q =", paste0(q, ","), "alpha =", paste0(alpha, ":"), "unobserved confounders (orthogonal to the covariates) that explain more than", paste0(100*rv_qa,"%"), "of the residual variance",
      "of both the treatment and the outcome are strong enough to bring the estimate to a range where it is no longer 'statistically different' from",
      paste0(h0), "(a bias of", paste0(100*q, "%"),"of the original estimate),",
      "at the significance level of alpha =", paste0(alpha, "."),
      "Conversely, unobserved confounders that do not explain more than", paste0(100*rv_qa,"%"), "of the residual variance",
      "of both the treatment and the outcome are not strong enough to bring the estimate to a range where it is no longer 'statistically different' from",
      paste0(h0,","),
      "at the significance level of alpha =", paste0(alpha,"."))
  cat("\n\n")

  bounds <- x$bounds
  if (!is.null(bounds)) {
    numeric <- sapply(bounds, is.numeric)
    bounds[numeric] <- lapply(bounds[numeric], round, digits = digits)
    names(bounds) <-  sapply(gsub("\\_", " ",  names(bounds)), .simpleCap)

    cat("Bounds on omitted variable bias:\n\n")
    cat("--The table below shows the maximum strength of unobserved confounders with association with the treatment and the outcome bounded by a multiple of the observed explanatory power of the chosen benchmark covariate(s).\n\n")
    print(bounds, row.names = FALSE)
  }
}


#' @rdname print.sensemakr
#' @return
#' The function \code{ovb_minimal_reporting} returns the LaTeX/HTML code invisibly in character form and also prints with
#' \code{\link{cat}} the LaTeX code. To suppress automatic printing, set \code{verbose = FALSE}.
#' @param format code format to print, either \code{latex} or \code{html}. The default html version
#' has some mathematical content that requires mathjax or equivalent library to parse.
#' If you need only html, use the option "pure_html".
#'
#' @param verbose if `TRUE`, the function prints the LaTeX code with \code{\link{cat}}
#' @export
ovb_minimal_reporting <- function(x, digits = 3, verbose = TRUE, format = c("latex", "html", "pure_html"), ...){
  if (!inherits(x, "sensemakr")) stop("Object needs to be of class sensemakr.")
  format <- match.arg(format)
  fun    <- switch(format,
                   latex = latex_table,
                   html  = html_table,
                   pure_html = html_table_no_mathjax)

  fun(x = x, digits = digits, verbose = verbose, ...)
}

latex_table <- function(x, digits = 3, verbose = TRUE, ...){
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
    footnote = paste0("\\hline \n",
                      "df = ", x$sensitivity_stats$dof, " & & ",
                      "\\multicolumn{5}{r}{ ",
                      "}")
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

  if (verbose) cat(table)

  return(invisible(table))
}

html_table <- function(x, digits = 3, verbose = TRUE, ...){

  # Let's begin
  table_settings = list(...)

  outcome_label = ifelse(is.null(table_settings[["outcome_label"]]),
                         all.vars(x$info$formula)[1],
                         table_settings[["outcome_label"]])

  treatment_label = ifelse(is.null(table_settings[["treatment_label"]]),
                           x$sensitivity_stats$treatment,
                           table_settings["treatment_label"])



  # Okay, static beginning
  table_begin = paste0("<table>\n")

  # Coeff header row
  coeff_header = paste0(

    "<thead>\n",
    "<tr>\n",
    '\t<th style="text-align:left;border-bottom: 1px solid transparent;border-top: 1px solid black"> </th>\n',
    '\t<th colspan = 6 style="text-align:center;border-bottom: 1px solid black;border-top: 1px solid black"> Outcome: ',
    outcome_label,'</th>\n',
    "</tr>\n",
    "<tr>\n",
    '\t<th style="text-align:left;border-top: 1px solid black"> Treatment </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> Est. </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> S.E. </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> t-value </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> $R^2_{Y \\sim D |{\\bf X}}$ </th>\n',
    paste0('\t<th style="text-align:right;border-top: 1px solid black">  $RV_{q = ',
           x$info$q, '}$ </th>\n'),
    paste0('\t<th style="text-align:right;border-top: 1px solid black"> $RV_{q = ',
           x$info$q, ", \\alpha = ",
           x$info$alpha, "}$ </th>\n"),
    "</tr>\n",
    "</thead>\n"
  )


  # Treatment result
  # Why paste and not sprintf? Easier to handle precision on the digits.
  coeff_results = paste0(
    "<tbody>\n <tr>\n",
    '\t<td style="text-align:left; border-bottom: 1px solid black"><i>',
    treatment_label, "</i></td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$estimate, digits), " </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$se, digits), " </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$t_statistic, digits)," </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$r2yd.x, digits), "\\% </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$rv_q, digits), "\\% </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$rv_qa, digits), "\\% </td>\n",
    "</tr>\n</tbody>\n")

  table_end <- "</table>"

  # Foonote row: Display benchmarks
  if (!is.null(x$bounds)) {
    row = x$bounds[1, , drop = FALSE]
    footnote = paste0('<tr>\n',
                      "<td colspan = 7 style='text-align:right;border-top: 1px solid black;border-bottom: 1px solid transparent;font-size:11px'>",
                      "Note: df = ", x$sensitivity_stats$dof, "; ",
                      "Bound ( ", row$bound_label, " ):  ",
                      "$R^2_{Y\\sim Z| {\\bf X}, D}$ = ",
                      100 * round(row$r2yz.dx, digits),
                      "\\%, $R^2_{D\\sim Z| {\\bf X} }$ = ",
                      100 * round(row$r2dz.x, digits),
                      "\\%",
                      "</td>\n",
                      "</tr>\n")
  } else {
    footnote = paste0('<tr>\n',
                      "<td colspan = 7 style='text-align:right;border-top: 1px solid black;border-bottom: 1px solid transparent;font-size:11px'>",
                      "Note: df = ", x$sensitivity_stats$dof, "; ",
                      "</td>\n",
                      "</tr>\n")
  }

  # Stick it all together
  table = paste0(table_begin,
                 coeff_header,
                 coeff_results,
                 footnote,
                 table_end)

  # Cat to output valid LaTeX for rmarkdown

  if (verbose) cat(table)

  return(invisible(table))
}



html_table_no_mathjax <- function(x, digits = 3, verbose = TRUE, ...){

  # Let's begin
  table_settings = list(...)

  outcome_label = ifelse(is.null(table_settings[["outcome_label"]]),
                         all.vars(x$info$formula)[1],
                         table_settings[["outcome_label"]])

  treatment_label = ifelse(is.null(table_settings[["treatment_label"]]),
                           x$sensitivity_stats$treatment,
                           table_settings["treatment_label"])



  # Okay, static beginning
  table_begin = paste0("<table style='align:center'>\n")

  # Coeff header row
  coeff_header = paste0(

    "<thead>\n",
    "<tr>\n",
    '\t<th style="text-align:left;border-bottom: 1px solid transparent;border-top: 1px solid black"> </th>\n',
    '\t<th colspan = 6 style="text-align:center;border-bottom: 1px solid black;border-top: 1px solid black"> Outcome: ',
    outcome_label,'</th>\n',
    "</tr>\n",
    "<tr>\n",
    '\t<th style="text-align:left;border-top: 1px solid black"> Treatment </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> Est. </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> S.E. </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> t-value </th>\n',
    '\t<th style="text-align:right;border-top: 1px solid black"> R<sup>2</sup><sub>Y~D|X</sub> </th>\n',
    paste0('\t<th style="text-align:right;border-top: 1px solid black">  RV<sub>q = ',
           x$info$q, '</sub> </th>\n'),
    paste0('\t<th style="text-align:right;border-top: 1px solid black"> RV<sub>q = ',
           x$info$q, ", &alpha; = ",
           x$info$alpha, "</sub> </th>\n"),
    "</tr>\n",
    "</thead>\n"
  )


  # Treatment result
  # Why paste and not sprintf? Easier to handle precision on the digits.
  coeff_results = paste0(
    "<tbody>\n <tr>\n",
    '\t<td style="text-align:left; border-bottom: 1px solid black"><i>',
    treatment_label, "</i></td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$estimate, digits), " </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$se, digits), " </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    round(x$sensitivity_stats$t_statistic, digits)," </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$r2yd.x, digits), "\\% </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$rv_q, digits), "\\% </td>\n",
    '\t<td style="text-align:right;border-bottom: 1px solid black">',
    100 * round(x$sensitivity_stats$rv_qa, digits), "\\% </td>\n",
    "</tr>\n</tbody>\n")

  table_end <- "</table>"

  # Foonote row: Display benchmarks
  if (!is.null(x$bounds)) {
    row = x$bounds[1, , drop = FALSE]
    footnote = paste0('<tr>\n',
                      "<td colspan = 7 style='text-align:right;border-bottom: 1px solid transparent;font-size:11px'>",
                      "Note: df = ", x$sensitivity_stats$dof, "; ",
                      "Bound ( ", row$bound_label, " ):  ",
                      "R<sup>2</sup><sub>Y~Z|X,D</sub> = ",
                      100 * round(row$r2yz.dx, digits),
                      "\\%, R<sup>2</sup><sub>D~Z|X</sub> = ",
                      100 * round(row$r2dz.x, digits),
                      "\\%",
                      "</td>\n",
                      "</tr>\n")
  } else {
    footnote = paste0('<tr>\n',
                      "<td colspan = 7 style='text-align:right;border-bottom: 1px solid transparent;font-size:11px'>",
                      "Note: df = ", x$sensitivity_stats$dof, "; ",
                      "</td>\n",
                      "</tr>\n")
  }

  # Stick it all together
  table = paste0(table_begin,
                 coeff_header,
                 coeff_results,
                 footnote,
                 table_end)

  # Cat to output valid LaTeX for rmarkdown

  if (verbose) cat(table)

  return(invisible(table))
}
