
# General Sensitivity Statistics ------------------------------------------

# robustness value --------------------------------------------------------


#' Creates orthogonal residuals
#'
#' @description
#' This function is an auxiliary function for simulation purposes.
#' It creates a vector of n standard normal random variables, residualizes
#' this vector against a matrix of covariates C, then standardizes the vector again.
#'
#'
#' @param n sample size.
#' @param C a numeric matrix with \code{n] rows and \code{p} columns.

#' @return
#'
#' The function returns a numeric vector orthogonal to C.
#'
#' @export
resid_maker <- function(n, C){
  e <- resid(lm(rnorm(n) ~ C))
  e  <- c(scale(e))
  return(e)
}
