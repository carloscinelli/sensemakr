#' @rdname make_table.sensemakr
#' @export
make_table = function(x, ...) {
  UseMethod("make_table")
}

#' @rdname make_table.sensemakr
#' @export
make_table.default = function(x, ...) {
  stop("`make_table` method is only defined for sensemakr objects.")
}

#' Produce a LaTeX summary of sensemakr output
#'
#' This function produces a LaTeX summary of `sensemakr` object output: it
#' briefly summarizes the observed effect, standard error, partial R^2,
#' and robustness values as well as partial R^2 for variables used as
#' benchmarks. This function directly outputs LaTeX table code which the user
#' can either copy into an external LaTeX file or directly output the table
#' via RMarkdown / knitr. To output the table via RMarkdown / knitr, please
#' place the `make_table` call in an RMarkdown chunk by itself and use the
#' 'results = "asis"' chunk option.
#'
#' Additional display parameters include `outcome_label`, which overrides the
#' outcome variable label, `treatment_label`, which overrides the treatment
#' variable label, and `benchmark_label`, which overrides the benchmark
#' variable label(s), `caption`, which adds a caption to the bottom of the
#' LaTeX table, and `label`, which adds a label to the bottom of the LaTeX
#' table.
#'
#' @param x A `sensemakr` object
#' @param digits A number indicating how many digits to display (percentage
#' outputs are rounded before multiplying by 100)
#' @param ... Additional display parameters governing the LaTeX table. See
#' description below for details.
#' @export
make_table.sensemakr = function(x,
                                digits = 3,
                                ...) {
  # Let's begin
  table_settings = list(...)
  # Override variable labels
  outcome_label = ifelse(is.null(table_settings[["outcome_label"]]),
                         all.vars(x$formula_outcome)[1],
                         table_settings[["outcome_label"]])
  treatment_label = ifelse(is.null(table_settings[["treatment_label"]]),
                           x$treatment_variable,
                           table_settings["treatment_label"])

  # Benchmark labels are going to be a little trickier.
  if(!is.null(table_settings[["benchmark_label"]])) {

    if(is.null(x$benchmark) ||
       nrow(x$benchmark) != length(table_settings[["benchmark_label"]])) {
         stop("Benchmark labels provided did not match number of benchmark ",
              "labels in `sensemakr` object.")
    }

    benchmark_labels = table_settings[["benchmark_label"]]
  } else {
    benchmark_labels = x$benchmark[, "variable"]
  }

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
    "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ & $RV$ ",
    "& $RV_{0.05}$ \\\\ \n",
    "\\hline \n"
  )

  # Treatment result
  # Why paste and not sprintf? Easier to handle precision on the digits.
  coeff_results = paste0(
    "\\textit{", treatment_label, "} & ",
    round(x$treatment_effect[1], digits), " & ",
    round(x$treatment_effect[2], digits), " & ",
    " & ",
    100 * round(x$r2_yd, digits), "\\% & ",
    100 * round(x$rv, digits), "\\% & ",
    100 * round(x$rv_t, digits), "\\% \\\\ \n")

  # Foonote row: Display benchmarks
  if(!is.null(x$benchmark)) {
    footnote_begin = paste0("\\hline \n",
                      "df = ", x$dof, " & & ",
                      "\\multicolumn{5}{r}{ ",
                      "")

    footnotes = lapply(
      seq.int(nrow(x$benchmark)),
      function(i) {
        row = x$benchmark[i, , drop = FALSE]

        paste0("\\small ",
               "\\textit{Bound (1 $\\times$ ", benchmark_labels[i], ")}: ",
               "$R^2_y$ = ",
               100 * round(row$r2y, digits),
               "\\%, $R^2_d$ = ",
               100 * round(row$r2d, digits),
               "\\%")
      })
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
