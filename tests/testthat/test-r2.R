context("test-r2.R")

test_that("r^2 helper", {
  expect_error(partial_r2())
  expect_error(partial_r2("hello world"))

  result = partial_r2(test_obj$model_outcome)

  expect_equal(length(result), 493)
  expect_equal(min(result), 1.903e-08, tolerance=1e-3)
  expect_equal(max(result), 0.10903, tolerance=1e-3)
})
