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


  ########################################
  # contour plot
  ########################################
  plot1_data <- plot(sense)

  expect_error(plot(sense,showvars='blahblah'))
  expect_error(plot(sense,showvars=c('blahblah')))

  plot(sense,showvars='masked',lim=0.02)
  plot(sense,showvars='all',lim=0.02)
  plot(sense,showvars=list('village','villageMngao','age'),lim=0.02)
  plot(sense,showvars=list('village','villageMngao','age'),lim=0.5)

  # a custom grouping term
  # sensemakr(...,group_list)
  # propogated to plot(...,showvars)

  test2 = sensemakr(model=model,treatment='directlyharmed',
                       group_list = list(c('village','female')))

  # notice single group term group_list = list(c('village','female')
  # must be concatenated as single character vector in showvars
  # 'village,female' arg of plot()
  # since the concatenated version is
  # the row.name returned from sensemakr()

  names(test2$benchmarks)
  (test2$benchmarks$benchmark_group)

  plot(test2,showvars=list('village,female','village','villageMngao','age'),lim=0.5)

  # 'village,female' does not show below
  # since incorrectly specified during showvars
  plot(test2,showvars=list('village , female','village','villageMngao','age'),lim=0.5)

  # deafult showvars='masked'
  plot(sense, lim=.2)
  plot2_data <- plot(sense, contour = "t-value")
  plot3_data <- plot(sense, contour = "lower bound")
  plot4_data <- plot(sense, contour = "upper bound")

  # with showvars='all'
  plot(sense, contour = "t-value",showvars='all')
  plot(sense, contour = "t-value",showvars='all',lim=0.02)
  plot(sense, contour = "lower bound",showvars='all',lim=0.02)
  plot(sense, contour = "upper bound",showvars='all',lim=0.02)

  # with showvars explicit
  plot(sense, contour = "t-value",
       showvars=list('village','villageMngao','age'))
  plot(sense, contour = "lower bound",
       showvars=list('village','villageMngao','age'))
  plot(sense, contour = "upper bound",
       showvars=list('village','villageMngao','age'))


  ########################################
  # worst-case plot
  ########################################
  plot5_data <- plot(sense, type = "worst-case")

  plot(sense, type = "worst-case",showvars='masked',lim=0.5)

  plot(sense, type = "worst-case",showvars='all',lim=0.5)

  plot(sense, type = "worst-case",lim=0.5,
       showvars=list('village','villageMngao','age'))




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
