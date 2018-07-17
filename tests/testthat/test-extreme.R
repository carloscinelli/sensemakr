context("test-extreme.R")



test_that("extreme plot", {

  lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
                  herder_dar + pastvoted + hhsize_darfur + female + village, data = darfur)

  # Using attributes
  ovb_extreme_plot(estimate = 2, se = 0.5, dof = 200)

  ovb_extreme_plot(estimate = 2, se = 0.5, dof = 200, r2d = 0.2)

  ovb_extreme_plot(lm.out, treatment = "directlyharmed")

  ovb_extreme_plot(lm.out, treatment = "directlyharmed",
                   benchmark_covariates = "female")

  ovb_extreme_plot(lm.out, treatment = "directlyharmed",
                   benchmark_covariates = "female",
                   kd = 1:3)

  ovb_extreme_plot(formula = peacefactor ~ directlyharmed + age + farmer_dar +
                     herder_dar + pastvoted + hhsize_darfur + female + village,
                   data = darfur,
                   treatment = "directlyharmed",
                   benchmark_covariates = "female",
                   kd = 1:3)
  # Using a sensemakr object
  # no_bench = sensemakr(formula = peacefactor ~ directlyharmed + age +
  #                        farmer_dar + herder_dar + pastvoted + hhsize_darfur +
  #                        female + village,
  #                      treatment = "directlyharmed",
  #                      data = darfur)
  # expect_error(extreme_plot(no_bench))

  # extreme_plot(test_obj)
  # plot(test_obj, type="extreme")
  #
  # # Check on manual override of rug.
  # expect_message(extreme_plot(test_obj, r2d = 0.05))
})
