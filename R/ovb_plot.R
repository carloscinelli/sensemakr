#' Traditional omitted variable bias plot
#'
#' This function produces a traditional omitted variable bias plot, which plots
#' the impact of an omitted variable on the relationship on interest contingent
#' on imbalance between treatment and control groups and a relationship between
#' the hypothetical variable and the outcome. We do not recommend using this
#' function, but provide it for compatibility with previous approaches. Please
#' note that this function is not exported and must be accessed through the
#' `sensemakr` package namespace. `ovb_plot` is a generic function to produce
#' omitted variable plots. The function invokes particular methods which depend
#' on the class of the first argument. For documentation on using `ovb_plot`
#' with `sensemakr` objects, check \link{ovb_plot.sensemakr}. For
#' documentation on using `ovb_plot` with direct numeric arguments, check
#' \link{ovb_plot.default}
#' @param ... Arguments which will pass through to the methods invoked.
#' @export
ovb_plot = function(...) {
  UseMethod("ovb_plot")
}



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



#' Traditional omitted variable bias plot
#'
#' This function produces a traditional omitted variable bias plot, which plots
#' the impact of an omitted variable on the relationship on interest contingent
#' on imbalance between treatment and control groups and a relationship between
#' the hypothetical variable and the outcome. We do not recommend using this
#' function, but provide it for compatibility with previous approaches.
#'
#' @param estimate An effect estimate.
#' @param lim A numeric vector of length 3 containing the beginning, end, and
#' increment of a sequence, forms the axes of the omitted variable bias plot
#' @param nlevels The number of contour lines to plot
#' @param col.contour A character string describing the color of the contour
#' lines
#' @param col.line A character string describing the color of the critical value
#' /threshold line
#' @param ... Additional graphics parameters
#' @param model An `lm` object. If specified, `estimate` will be extracted from
#' this model
#' @param covariate An quoted character string describing the treatment variable
#' in the `model` object passed.
#'
#' @importFrom graphics contour points text
#' @export
ovb_plot.default = function(estimate = NULL,
                            lim = c(0, 0.4, 0.01),
                            nlevels = 20,
                            col.contour = "grey40",
                            col.line = "red",
                            ...,
                            model = NULL, covariate = NULL) {

  # In general we want the user to pass us an estimate. But if they would
  # rather pass us a model, let's use the model/covariate to fill the estimate.
  if(!is.null(model) && !is.null(covariate) && "lm" %in% class(model)) {
    estimate = coef(summary(model))[covariate, "Estimate"]
  }

  # Error if we don't have an estimate or if it's non-numeric
  if(is.null(estimate)) {
    stop("You must supply either a `model` and `covariate` to extract an ",
         "estimate or a directly supplied `estimate` argument")
  }
  if(!is.numeric(estimate)) {
    stop("The estimated effect must be numeric.")
  }

  # Error if our grid limit is wrong.
  if(any(!is.numeric(lim) | lim < 0 | lim > 1) ||
     lim[2] <= lim[1] ||
     length(lim) != 3) {
    stop("Plot grid must consist of three numbers: lim = c(start, end, by)")
  }

  # Set up the grid for the contour plot
  grid_values = seq(lim[1], lim[2], by = lim[3])
  z_axis = estimate - outer(grid_values, grid_values)

  # Aesthetic: Override the 0 line; basically, check which contour curve is
  # the zero curve, and override that contour curve with alternate aesthetic
  # characteristics
  default_levels = pretty(range(z_axis), nlevels)
  line_color = ifelse(default_levels == 0,
                      col.line,
                      col.contour)
  line_type = ifelse(default_levels == 0,
                     2, 1)
  line_width = ifelse(default_levels == 0,
                      2, 1)

  # Plot contour plot:
  contour(
    grid_values, grid_values, z_axis, nlevels = nlevels,
    xlab = paste0("Hypothetical imbalance of omitted variable between ",
                  "treatment and control"),
    ylab = "Hypothetical impact of omitted variable on dependent variable",
    cex.main = 1,
    cex.lab = 1,
    cex.axis = 1,
    col = line_color,
    lty = line_type,
    lwd = line_width,
    ...)

  # Add the point of the initial estimate.
  points(0, 0, pch = 17, col = "black", cex = 1)
  text(0.023, 0.01,
       paste0("unadjusted\n(",
              signif(estimate, 2),
              ")"),
       cex = 1)
}
