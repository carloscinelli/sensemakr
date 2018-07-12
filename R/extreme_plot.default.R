#' Extreme scenario plot
#'
#' Produces an extreme scenario plot: a plot where unobserved confounders
#' explain all the left-out residual variance of the outcome.
#'
#' @param estimate An estimated effect magnitude
#' @param se The standard error of the estimated effect magnitude
#' @param dof The residual degrees of freedom of the regression producing the
#' effect estimate
#' @param r2d Partial R^2 with respect to the treatment
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the bound plot
#' @param scenarios A vector of proportions of the residual variance explained
#' by the unobserved confounder. Defaults to `c(1, 0.8, 0.5)`
#' @param cex.legend A scaling factor for the legend text.
#' @param ... Additional graphical parameters
#'
#' @importFrom graphics abline lines legend rug plot
#' @export
extreme_plot.default = function(estimate, se, dof, r2d = NULL,
                                lim = NULL,
                                scenarios = c(1, 0.8, 0.5),
                                cex.legend = 0.5, ...) {

  if(is.null(lim)) {
    if(is.null(r2d)) {
      stop("If `lim` is not provided, `r2d` must be to automatically pick a ",
           "plot limit.")
    }
    lim = max(r2d, na.rm = TRUE) + 0.1
  }

  # x-axis values: R^2 of confounder(s) with treatment
  r2d_values = seq(0, lim, by = 0.001)

  # Iterate through scenarios:
  for(i in seq.int(length(scenarios))) {
    y = estimate -
      bias_in_r2(r2y = scenarios[i],
                 r2d = r2d_values,
                 se = se,
                 dof = dof)



    # Initial plot
    if(i == 1) {
      plot(
        r2d_values, y, type="l", bty = "L",
        xlab = paste0("Hypothetical partial R^2 of unobserved ",
                      "confounder(s) with treatment"),
        ylab = "Adjusted effect estimate",
        ...)
      abline(h = 0, col = "red", lty = 5)
    } else {
      # Add plot lines
      lines(r2d_values, y, lty = i + 1)
    }

  }

  legend(
    x = "topright",
    inset = 0.05,
    lty = c(seq_len(length(scenarios)), 5),
    col = c(rep("black", length(scenarios)),
            "red"),
    bty = "n",
    legend = paste0(scenarios * 100, "%"),
    ncol = length(scenarios) + 1,
    title = paste0("Hypothetical partial R^2 of \n",
                   "unobserved confounder(s) with outcome"),
    cex = cex.legend
  )

  rug(x = r2d, col = "red", lwd = 2)
}
