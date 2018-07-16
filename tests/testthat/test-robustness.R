context("test-robustness_value.R")

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

  expect_error(robustness_value(t_statistic = 2.5,
                                dof = 200,
                                q = 2))

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
  # now we accept more than one t
  # expect_error(robustness_value(t_statistic = c(2.5, 3.5),
                                # dof = 200))

  # With a model now we accept the full model
  # expect_error(robustness_value(model = test_obj$model_outcome))

  expect_error(robustness_value(model = "hello world",
                                covariate = "test"))

  # Works with a model
  expect_equal(
    unname(robustness_value(
      model = test_obj$model_outcome,
      covariate = "directlyharmed")),
    0.13877, tolerance = 1e-3)

  # Works with direct t from sensemakr object (duh)
  expect_equal(
    unname(robustness_value(
      t_statistic = test_obj$t["directlyharmed"],
      dof = test_obj$dof
    )),
    0.13877, tolerance = 1e-3)
})
