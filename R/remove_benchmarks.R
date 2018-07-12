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
