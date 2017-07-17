##' @title Senstivity abalysis of linear models
##'
##' @description Description.
##'
##' @param D  character vector with the treatment variable.
##' @param X  character vector with the covariates for benchmarking.
##' @param ... extra arguments
##' @return The function returns an object of class 'sensemakr' which is a list with the main
##' results for sensitivity analysis, namely:
##' \item{Treat Stats}{
##'    A list with the main statistics of the treatment estiamate which are needed for computing the sensitivity.
##'    \itemize{
##'       \item name
##'       \item estimate
##'       \item se
##'       \item df
##'       }
##'   }
##' \item{Benchmarks}{Benchmark values for the covariates listed on paramter X
##'     \itemize{
##'     \item R2 a data.frame with becnhmark values R2.
##'     \item SD a data.frame with benchmark values
##'     \item natural a data.frame with...
##'     }
##'   }
##'
##' @examples
##' # worked out example
##'
##' @references
##' Cite paper(s)
##'
##' @export
sensemakr <- function(...){
  UseMethod("sensemakr")
}


##' @param model the model.
##' @name sensemakr
sensemakr.lm <- function(...){
  # stats <- get stats()
  # benchmarks <- get benchmarks()
  # compute bias and include ob data.frames
  # returns pretty list with class "sensemade"

}


# Input: takes an lm object and the treatment name.
# It returns a data.frame with the names of the treatment, the estimate, the standard error,
# and the degrees of freedom.
#
getstats <- function(...){

}

c("asd", "Das", "das")
list("sda", together = c("sda","dasda"))
# Input: takes an lm object + treatment name + covariates name
# covariates names -> if a single character vector, than each variable is a benchmark
#                  -> if a list of character vectors, then group by list (future work)
# Output: three data.frames with benchmarks for R2, SD and natural.
#       - data.frame contains: Names, R2y or delta, R2d or gamma
benchmarkr <- function(...){

}

##' @title Computes  bias caused by unobserved confounder
##' @description  This function computes the bias caused by an unobserved confounder
##' with certain characteristics.
##'
##' @param estimate original original  treatment effect estimate
##' @param se       standard error of original  treatment effect estimate
##' @param df       degrees of freedom of the original linear model
##' @param R2d      hypothetical partial R2 of the confounder with the treatment
##' @param R2y      hypothetical partial R2 of the confounder with the outcome
getbiasR2 <- function(...){

}

# mike test push pull
