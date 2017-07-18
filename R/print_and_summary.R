# prints ------------------------------------------------------------------

print.sensemade <- function(x, ...){

  cat("Sensitivity Analysis for the linear model:\n\n",
      as.character(x$model$call), "\n\n")
  str(x, max.level = 1)
}


summary.sensemade <- function(object, ...){
  # bunch of useful things
  # return list with several useful things
  # returns a obj of class summary.sensemade

}

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

