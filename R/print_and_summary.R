# prints ------------------------------------------------------------------

##' @export
print.sensemade <- function(x, ...){

  cat("Sensitivity Analysis\n\n")
  cat("Model:", trimws(deparse(formula(x$info$model))), "\n\n")
  cat("Treatment:", x$info$treatment, "\n")
  cat("Outcome:", x$info$outcome, "\n\n")
  cat("Object content:\n")
  str(x, max.level = 1)
  cat("\n For more, use plot and summary.")
}


##' @export
summary.sensemade <- function(object, ...){
  # bunch of useful things
  # return list with several useful things
  # returns a obj of class summary.sensemade

}

##' @export
print.summary.sensemade <- function(x, ...){
 # pretty print for the summary
}

# # this is the idea for the sensemade
# model <- lm(mtcars)
# model
# summarymodel <- summary(model)
# summarymodel
# stats:::summary.lm
# stats:::print.summary.lm


# bunch of useful functions for summary

