#' Traditional omitted variable bias plot
#'
#' This function produces a traditional omitted variable bias plot, which plots
#' the impact of an omitted variable on the relationship on interest contingent
#' on imbalance between treatment and control groups and a relationship between
#' the hypothetical variable and the outcome. We do not recommend using this
#' function, but provide it for compatibility with previous approaches.
#'
#' @param x A `sensemakr` object.
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the omitted variable bias plot
#' @param nlevels The number of contour lines to plot
#' @param col.contour A character string describing the color of the contour
#' lines
#' @param col.line A character string describing the color of the critical value
#' /threshold line
#' @param ... Additional graphics parameters
#'
#' @export
ovb_plot.sensemakr = function(x,
                            lim = c(0, 0.4, 0.01),
                            nlevels = 20,
                            col.contour = "grey40",
                            col.line = "red",
                            ...) {

  ovb_plot.default(
    estimate = x$treatment_effect[1],
    lim = lim,
    nlevels = nlevels,
    col.contour = col.contour,
    col.line = col.line,
    ...)
}
