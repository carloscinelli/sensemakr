context("Manually testing basic functions")

test_that("numericals tests",{
  expect_equal(partial_r2(t = 2, dof = 10), 2^2/(2^2 + 10))

  expect_equal(partial_f(t = 2, dof = 10), 2/sqrt(10))

  expect_equal(partial_f2(t = 2, dof = 10), 2^2/10)

  expect_equal(bias(se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4),
               3*sqrt(100)*sqrt(0.4*0.3/(1 - 0.3)))

  expect_equal(adjusted_estimate(estimate = 2, se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4),
               2 - 3*sqrt(100)*sqrt(0.4*0.3/(1 - 0.3)))

  expect_equal(adjusted_estimate(estimate = 2, se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4, reduce = FALSE),
               2 + 3*sqrt(100)*sqrt(0.4*0.3/(1 - 0.3)))

  expect_equal(adjusted_se(se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4),
               3*sqrt(100/99)*sqrt((1 - 0.4)/(1 - 0.3)))

  expect_equal(adjusted_t(estimate = 2, se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4),
               (2 - 3*sqrt(100)*sqrt(0.4*0.3/(1 - 0.3)))/(3*sqrt(100/99)*sqrt((1 - 0.4)/(1 - 0.3))))

  expect_equal(adjusted_t(estimate = 2, se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4, reduce = F),
               (2 + 3*sqrt(100)*sqrt(0.4*0.3/(1 - 0.3)))/(3*sqrt(100/99)*sqrt((1 - 0.4)/(1 - 0.3))))


  expect_equal(c(robustness_value(t = 2, dof = 10)), 0.5*(sqrt((2/sqrt(10))^4 + 4*((2/sqrt(10))^2)) - (2/sqrt(10))^2))

  expect_equal(group_partial_r2(F.stats = 10, dof = 100, p = 4), 10*4/(10*4 + 100))

  expect_equivalent(partial_r2(t_statistic = 1.89, dof = 1121), 0.0032, tolerance = 1e-4)
  expect_equivalent(robustness_value(t_statistic = 1.89, dof = 1121), 0.055, tolerance = 1e-2)

  expect_equivalent(partial_r2(t_statistic = 2.11, dof = 1115), 0.004, tolerance = 1e-4)
  expect_equivalent(robustness_value(t_statistic = 2.11, dof = 1115), 0.061, tolerance = 1e-2)

  expect_equivalent(partial_r2(t_statistic = 37.5, dof = 983), 0.59, tolerance = 1e-2)
  expect_equivalent(robustness_value(t_statistic = 37.5, dof = 983), 0.68, tolerance = 1e-2)

  expect_equivalent(partial_r2(t_statistic = 17, dof = 983), 0.23, tolerance = 1e-2)
  expect_equivalent(robustness_value(t_statistic = 17, dof = 983), 0.415, tolerance = 1e-2)
})

test_that("print tests",
          {
            print_rv <- c("[1] 0.463325", "Parameters: q = 1")
            expect_equal(capture.output(robustness_value(t = 2, dof = 10)), print_rv)

            print_rvqa <- c("[1] 0", "Parameters: q = 1, alpha = 0.05 ")
            expect_equal(capture.output(robustness_value(t = 2, dof = 10, alpha = 0.05)), print_rvqa)
          }
          )


test_that("input tests", {
  expect_error(partial_r2("text"))
  expect_error(partial_f("text"))
  expect_error(robustness_value("text"))
  expect_error(bias("text"))
  expect_error(adjusted_estimate("text"))
  expect_error(adjusted_t("text"))

  expect_error(sensitivity_stats(estimate = 2, se  =  -2))
  expect_error(sensitivity_stats(estimate = "hey", se  =  -2))
  expect_error(sensitivity_stats(estimate = 2, se  =  "hey"))
  expect_error(sensitivity_stats(estimate = 2, se  =  100, dof = -2))

  expect_error(adjusted_estimate(estimate = 2, se = 3, dof = 100, reduce = "nope"))
  expect_error(adjusted_estimate(estimate = c(2,3), se = 3, dof = 100))
  expect_error(adjusted_t(estimate = c(2,3), se = 3, dof = 100))
})



