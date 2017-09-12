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


  sense <- sensemakr(model=model, treatment="directlyharmed")
  str(sense$benchmarks)


  ########################################
  # contour plot
  ########################################
  plot1_data <- plot(sense)

  expect_error(plot(sense,showvars='blahblah'),
               'You have supplied an incompatible "showvars" option')

  expect_error(plot(sense,showvars=c('blahblah')),
               'You have supplied an incompatible "showvars" option')

  plot(sense,showvars='masked')
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

  plot(test2,lim=0.03)
  plot(test2,lim=0.5)

  #################
  # below
  # female masked
  # village made it thru
  # user speced
  # sensemakr(...,group_list = list(c('village','female'))
  #################

  # 'village,female' is a custom user speced group
  # 'female' is part of that group and is default masked
  # in plot(showvars='masked')
  # but 'village' term still makes it thru
  # because village itself is a group of 'factor levels'
  # we internally enforced factors to be ploted in showvars
  # that's why it appears even though 'masked' and
  # village is part of 'village,female'

  plot(test2,lim=0.02)
  plot(test2)

  names(test2$benchmarks)
  head(test2$benchmarks$benchmark_masked)
  head(test2$benchmarks$benchmark_group)

  # group to group masking not auto-supported by 'showvars'
  # design matrix to group masking is auto-supported by 'showvars'
  # see this edge case when

  # edgecase = sensemakr(group_list=list(c('village','female')))
  # plot(edgecase,showvars='masked')

  # 'village,female' is plotted
  # 'female' is not plotted, but 'village' (group) is plotted
  # NOTE: the factor levels of 'village' are not plotted

  # reason is, 'village' itself is a standalone group
  # hence present in (edgecase$benchmarks$benchmarks_group)

  # 'female' was in (edgecase$benchmarks$benchmark_eachvar)
  # whose elements qualify to be masked
  # notice, the factor levels of 'village' are masked
  # since they are also in (edgecase$benchmarks$benchmark_eachvar)


  # keep this behavior as is, force user to specify custom showvars
  # if they want 'village,female' but not 'village'
  # if they want to mask specific terms in
  # 'out_sensemakr$benchmarks$benchmark_group'
  # they must do it explicily themselves

  # eg

  plot(test2,showvars=list('village,female'),lim=0.5)

  # so make clear that 'masked' does not apply
  # (edgecase$benchmarks$benchmarks_group)

  # it DOES apply to
  # (edgecase$benchmarks$benchmarks_eachvar)



  plot(test2,showvars=list('village,female'),lim=0.5)

  # source of below error was jitter label
  # deprecated the 'jitter' feature

  # Error in if (xx != 0) xx/10 else z/10 : argument is of length zero
  # In addition: Warning messages:
  #   1: In min(x) : no non-missing arguments to min; returning Inf
  # 2: In max(x) : no non-missing arguments to max; returning -Inf
  # 3: In is.factor(x) : NaNs produced
  plot(test2,showvars=list('village,female','village'),lim=0.5)

  plot(test2,showvars=list('village'))

  # no error, source of error was jitter label

  plot(test2,showvars=list('village,female','village','villageMngao'),lim=0.5)

  plot(test2,showvars=list('village,female','village','villageMngao','age'),lim=0.5)

  # 'village,female' does not show below
  # since incorrectly specified during showvars
  plot(test2,showvars=list('village , female','village','villageMngao','age'),lim=0.5)

  # deafult showvars='masked'
  plot(sense, lim=.2)
  plot2_data <- plot(sense, contour = "t-value")
  plot3_data <- plot(sense, contour = "lower-limit")
  plot4_data <- plot(sense, contour = "upper-limit")

  # with showvars='all'
  plot(sense, contour = "t-value",showvars='all')
  plot(sense, contour = "t-value",showvars='all',lim=0.02)
  plot(sense, contour = "lower-limit",showvars='all',lim=0.02)
  plot(sense, contour = "upper-limit",showvars='all',lim=0.02)

  # with showvars explicit
  plot(sense, contour = "t-value",
       showvars=list('village','villageMngao','age'))
  plot(sense, contour = "lower-limit",
       showvars=list('village','villageMngao','age'))
  plot(sense, contour = "upper-limit",
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
