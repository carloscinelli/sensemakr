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

  # invert value is not logical
  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                invert = 1))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                invert = "hello"))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 1,
                                invert = c(TRUE,FALSE)))

  # q can be greater than 1
  robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 2)

  robustness_value(t_statistic = 2.5,
                   dof = 200,
                   q = 2,
                   invert = TRUE)

  # vectors for t-statistic/dof are allowed, but not for q
  robustness_value(t_statistic = c(2.5, 3),
                   dof = c(200,10),
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

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = -10,
                                q = 1,
                                alpha = 0.05))

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 10,
                                q = -1,
                                alpha = 0.05))

  expect_error(robustness_value(t_statistic = c(2.5,3),
                                dof = c(10,10),
                                q = c(1,1),
                                alpha = 0.05))

  expect_error(robustness_value(model = "hello world",
                                covariate = "test"))
})
