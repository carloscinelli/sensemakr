context("test-r2.R")

f  <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + female + village
lm.out  <- lm(f, data = darfur)

test_that("partial_r2", {
  expect_error(partial_r2())
  expect_error(partial_r2("hello world"))

  result = partial_r2(lm.out)

  expect_equal(length(result), 493)
  expect_equal(min(result), 1.903e-08, tolerance = 1e-3)
  expect_equal(max(result), 0.10903, tolerance = 1e-3)

})
