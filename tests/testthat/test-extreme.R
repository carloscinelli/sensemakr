context("test-extreme.R")

test_that("extreme plot", {
  # Using attributes
  expect_error(extreme_plot(estimate = 2, se = 0.5, dof = 200))
  extreme_plot(estimate = 2, se = 0.5, dof = 200, r2d = 0.2)

  # Using a sensemakr object
  no_bench = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur)
  expect_error(extreme_plot(no_bench))

  test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur,
                       benchmark = "female")
  extreme_plot(test_obj)
  plot(test_obj, type="extreme")

  # Check on manual override of rug.
  expect_message(extreme_plot(test_obj, r2d = 0.05))
})
