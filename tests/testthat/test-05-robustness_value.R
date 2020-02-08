context("test-robustness_value.R")


test_that("robustness value", {
  # Empty call
  expect_error(robustness_value())

  data("darfur")
  # runs regression model
  model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)

  expect_error(robustness_value(model, 2))
  # q out of bounds
  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = "hello"))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = c(0.5, 1)))

  # q can be greater than 1
  robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 2)

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = -0.5))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                alpha = -0.5))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                alpha = "hello"))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                alpha = 2))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                alpha = c(0.3, 0.3)))

  expect_error(robustness_value(model = "hello world",
                                covariate = "test"))
})
