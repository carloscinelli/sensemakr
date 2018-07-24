context("test-robustness_value.R")


f  <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + female + village
lm.out  <- lm(f, data = darfur)
t <- coef(summary(lm.out))["directlyharmed", "t value"]

test_that("robustness value", {
  # Empty call
  expect_error(robustness_value())

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

  # Works with a model
  expect_equal(
    as.numeric(robustness_value(
      model = lm.out,
      covariates = "directlyharmed")),
    0.13877, tolerance = 1e-3)

  # Works with direct t from sensemakr object (duh)
  expect_equal(
    as.numeric(robustness_value(
      t_statistic = t,
      dof = lm.out$df.residual
    )),
    0.13877, tolerance = 1e-3)
})
