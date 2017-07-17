# summary/plot functions

##' @title Sensitivity plots
##' @description Several sensitivity plots.
##'
##' @param x sensemade object
##' @param type type of the plot
##' @param ... extra arguments
##'
##'
##' @return a ggplot object with the plot.
plot.sensemade <- function(x, type = "xxx", ...){
  switch(type,
    contour = contourplot(x, ...),
    worstcase = worstcaseplot(x, ...)
  )
}

contourplot <- function(...){
  #

}


worstcaseplot <- function(...){
  #

}

print.sensemade <- function(x, ...){
  # pretty print of the object
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

