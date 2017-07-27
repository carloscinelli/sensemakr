context("Testing group features of sensemakr()")

test_that("Testing group terms", {

  rm(list = ls())

  library(sensemakr)
  data("darfur",package='sensemakr')

  # model the user wants to run sensitivity analysis
  mod_test  <- lm(peacefactor ~ directlyharmed + female + village, data = darfur)


  test = sensemakr.lm(model=mod_test,treatment='directlyharmed')

  str(test,max.level=1)
  str(test$benchmarks,max.level=1)

  # group results of 'village'
  capture_output(head(test$benchmarks$benchmark_R2_group),print=TRUE)

  # low level coef of village 'levels'
  capture_output(head(test$benchmarks$benchmark_R2),print=TRUE)

  # user supplied group_list for arbitrary grouping of terms
  # each list entry is a character vector.
  # elements of a character vector are simultaneously with-held

  test2 = sensemakr.lm(model=mod_test,treatment='directlyharmed',
                       group_list = list(c('village','female')))

  str(test2$benchmarks,max.level=1)

  # village AND female simultaneously with-held
  capture_output(head(test2$benchmarks$benchmark_R2_group),print=TRUE)

  # low level coef of village 'levels' still there
  capture_output(head(test2$benchmarks$benchmark_R2),print=TRUE)

  # so, can 'mask' later in print/summary/plot methods

  # test plots for groups
  plot_group_1 <- plot(test)
  plot_group_2 <- plot(test2)

})

