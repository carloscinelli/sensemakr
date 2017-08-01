context("Checking plots")

test_that("Testing plots", {
  rm(list = ls())

  # library
  library(sensemakr)

  # loads data
  data("darfur")

  # fits model
  model  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                 pastvoted + hhsize_darfur + female + village, data = darfur)

  # benchmark variables
  # X = c("herder_dar", "female", "age", "pastvoted", "farmer_dar")

  # runs benchmarking etc
  sense <- sensemakr(model=model, treatment="directlyharmed")
  str(sense$benchmarks)

  # plots

  ## contour plot
  plot1_data <- plot(sense)



  plot1_data_showvars <- plot(sense,showvars=showvars)

  expect_error(plot(sense,showvars='blahblah'))
  expect_error(plot(sense,showvars=c('blahblah')))

  plot(sense,showvars='masked',lim=0.02)
  plot(sense,showvars='all',lim=0.02)
  plot(sense,showvars=list('village','villageMngao','age'),lim=0.02)
  plot(sense,showvars=list('village','villageMngao','age'),lim=0.5)



  plot(sense, lim=.2)
  plot2_data <- plot(sense, contour = "t-value")
  plot3_data <- plot(sense, contour = "lower bound")
  plot4_data <- plot(sense, contour = "upper bound")
  ## worst-case plot
  plot5_data <- plot(sense, type = "worst-case")

  # got rid of 'X' argument in sensemakr(...,X)

  # mike stress test

  # error if X is just a single element vector
  # X = c("female")

  # runs benchmarking etc
  # sense <- sensemakr(model=model, treatment="directlyharmed", benchmarks=X)

  # if X is two element vector, then text labels and points not aligned
  # point position is the same, but labels shifted extremely up
  # X = c("female", "age")
  # sense <- sensemakr(model=model, treatment="directlyharmed", benchmarks=X)
  # plot1_data <- plot(sense)


})
