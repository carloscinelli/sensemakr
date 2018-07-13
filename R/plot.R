#' Sensemakr plotting functions
#'
#' This is a generic method which dispatches to various plotting functions for
#' `sensemakr` objects. By default, the plotting function chosen is
#' \link{contour_plot.sensemakr}. Using the `type` argument, users can also
#' select \link{extreme_plot.sensemakr} and \link{ovb_plot.sensemakr} plots.
#'
#' @param x A sensemakr object
#' @param type A character string reading "contour", "extreme", or "ovb".
#' @param ... Further plotting arguments described in documentation for
#' \link{contour_plot.sensemakr}, \link{extreme_plot.sensemakr}, or
#' \link{ovb_plot.sensemakr}.
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
#' # Contour plot
#' plot(sense.out)
#' plot(sense.out, type="contour")
#'
#' # Extreme Scenario plot
#' plot(sense.out, type="extreme")
#'
#' # Traditional, unscaled OVB plot
#' plot(sense.out, type="ovb")
#'
#' @export
plot.sensemakr = function(x, type = "contour", ...) {
  if(is.null(type) || !type %in% c("contour", "extreme", "ovb")) {
    stop("`type` argument to `plot.sensemakr` must be 'contour', 'extreme', ",
         "or 'ovb'.")
  }

  # Call the dispath function of interest
  switch(type,
         "contour" = dispatch_contour,
         "extreme" = dispatch_extreme,
         "ovb" = dispatch_ovb)(x, ...)
}

dispatch_contour = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  contour_plot(x, ...)
}

dispatch_extreme = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  extreme_plot(x, ...)
}

dispatch_ovb = function(x, ...) {
  # Use dispatcher rather than direct call so we can allow modifying call if
  # necessary
  ovb_plot(x, ...)
}
