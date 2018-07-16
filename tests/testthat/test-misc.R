context("test-misc.R")

test_that("r^2 helper functions", {
  # Math sanity check
  bias = bias_in_r2(r2y = 0.5, r2d = 0.2, se = 0.3, dof = 200)
  se = se_r2(r2y = 0.5, r2d = 0.2, se = 0.3, dof = 200)
  t_result = t_r2(estimate = 4, r2y = 0.5, r2d = 0.2, se = 0.3, dof = 200)

  expect_equal(bias, 1.5, tolerance=1e-3)
  expect_equal(se, 0.2377, tolerance=1e-3)
  expect_equal(t_result, 10.5145, tolerance=1e-3)

  # Errors
  # Bad estimate
  expect_error(t_r2(estimate = "hello",
                    r2y = 0.5, r2d = 0.2, se = 0.3, dof = 200))

  # Bad R2Y
  expect_error(bias_in_r2(r2y = "hello", r2d = 0.2, se = 0.3, dof = 200))
  expect_error(bias_in_r2(r2y = -1, r2d = 0.2, se = 0.3, dof = 200))
  expect_error(bias_in_r2(r2y = 2, r2d = 0.2, se = 0.3, dof = 200))
  # Bad R2D
  expect_error(bias_in_r2(r2y = 0.2, r2d = "hello", se = 0.3, dof = 10))
  expect_error(bias_in_r2(r2y = 0.2, r2d = -1, se = 0.3, dof = 10))
  expect_error(bias_in_r2(r2y = 0.2, r2d = 2, se = 0.3, dof = 10))
  # Bad SE
  expect_error(bias_in_r2(r2y = 0.2, r2d = 0.2, se = -0.3, dof = 10))
  # Bad DOF
  expect_error(bias_in_r2(r2y = 0.2, r2d = 0.2, se = 0.3, dof = -10))

})

test_that("weird regression model with r^2", {
  mean = rnorm(10)
  error = rnorm(10, sd = 0.3)
  y = mean + error
  constant = rep(2, 10)
  group = as.factor(c(rep(1:5, 2)))
  factor = as.factor(1:10)

  multicol_model = lm(y ~ group + constant)
  expect_warning(partial_r2(multicol_model))

  saturated_model = lm(y ~ factor)
  expect_error(partial_r2(saturated_model))
})

test_that("broken bound calculator?", {
  # TODO: This NaNs because r2_zx ends up > 1, forcing a negative
  # square root in r2_yz. Hmmm?
  expect_error(sensemakr:::bound_calculator(
    r2y = 0.09862925, r2d = 0.3349933,
    multiplier_y = 2, multiplier_d = 2))
})

test_that("misc plot dispatcher", {
  expect_error(plot(test_obj, type="invalid"))
})

test_that("misc table tests", {
  expect_error(make_table(test_obj,
                          benchmark_label = c("hello", "world")))

  expect_error(make_table("hello world"))

  make_table(test_obj)
})
