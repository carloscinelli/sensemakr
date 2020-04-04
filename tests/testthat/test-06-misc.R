context("test-misc.R")

test_that("r^2 helper functions", {
  # Math sanity check
  bias = bias(r2yz.dx = 0.5, r2dz.x = 0.2, se = 0.3, dof = 200)
  se = adjusted_se(r2yz.dx = 0.5, r2dz.x = 0.2, se = 0.3, dof = 200)
  t_result = adjusted_t(estimate = 4, r2yz.dx = 0.5, r2dz.x = 0.2, se = 0.3, dof = 200)

  expect_equal(bias, 1.5, tolerance = 1e-3)
  expect_equal(se, 0.2377, tolerance = 1e-3)
  expect_equivalent(t_result, 10.5145, tolerance = 1e-3)

  # Errors
  # Bad estimate
  expect_error(adjusted_t(estimate = "hello",
                    r2yz.dx = 0.5, r2dz.x = 0.2, se = 0.3, dof = 200))

  # Bad r2yz.dx
  expect_error(bias(r2yz.dx = "hello", r2dz.x = 0.2, se = 0.3, dof = 200))
  expect_error(bias(r2yz.dx = -1, r2dz.x = 0.2, se = 0.3, dof = 200))
  expect_error(bias(r2yz.dx = 2, r2dz.x = 0.2, se = 0.3, dof = 200))
  # Bad r2dz.x
  expect_error(bias(r2yz.dx = 0.2, r2dz.x = "hello", se = 0.3, dof = 10))
  expect_error(bias(r2yz.dx = 0.2, r2dz.x = -1, se = 0.3, dof = 10))
  expect_error(bias(r2yz.dx = 0.2, r2dz.x = 2, se = 0.3, dof = 10))
  # Bad SE
  expect_error(bias(r2yz.dx = 0.2, r2dz.x = 0.2, se = -0.3, dof = 10))
  # Bad DOF
  expect_error(bias(r2yz.dx = 0.2, r2dz.x = 0.2, se = 0.3, dof = -10))

})

test_that("weird regression model with r^2", {
  mean = rnorm(10)
  error = rnorm(10, sd = 0.3)
  y = mean + error
  constant = rep(2, 10)
  group = as.factor(c(rep(1:5, 2)))
  factor = as.factor(1:10)

  # multicol_model = lm(y ~ group + constant)
  # expect_warning(partial_r2(multicol_model))

  saturated_model = lm(y ~ factor)
  expect_error(partial_r2(saturated_model))
})
