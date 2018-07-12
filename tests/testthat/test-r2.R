context("test-r2.R")

test_that("r^2 helper", {
  expect_error(r_squared_helper())
  expect_error(r_squared_helper("hello world"))

  test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur,
                       benchmark = "female")

  result = r_squared_helper(test_obj$model_outcome)

  expect_equal(length(result), 493)
  expect_equal(min(result), 1.903e-08, tolerance=1e-3)
  expect_equal(max(result), 0.10903, tolerance=1e-3)
})
