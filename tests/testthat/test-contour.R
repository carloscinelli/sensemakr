context("test-contour.R")

test_that("contour plot tests", {
  # Missing everything
  expect_error(contour_plot())

  # Nonnumeric estimate
  expect_error(contour_plot(estimate = "hello"))
  # Missing estimate
  expect_error(contour_plot(estimate = 2))
  # Missing DOF
  expect_error(contour_plot(estimate = 2, se = 0.5))

  # Misspecified lim
  expect_error(contour_plot(estimate = 2, se = 0.5,
                          dof = 200,
                          lim = c(1, 1, 1, 1)))
  expect_error(contour_plot(estimate = 2, se = 0.5,
                          dof = 200,
                          lim = c(1, 0, 0.2)))
  expect_error(contour_plot(estimate = 2, se = 0.5,
                          dof = 200,
                          lim = c(-1, 2, 0.3)))

  # Misspecified multipliers
  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          multipliers_y = c(1, 2, 3, 4),
                          multipliers_d = c(1, 2, 3)))

  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          multipliers_y = c(-1, 2, 3)))

  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          multipliers_y = c("hello", 2, 3)))

  # Misspecified r2d
  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2d = -1,
                          r2y = 0.3))

  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2d = "hello",
                          r2y = 0.3))

  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2d = c(0.5, 0.8),
                          r2y = 0.3))

  # Misspecified r2y
  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2d = 0.3,
                          r2y = -1))

  expect_error(contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2d = 0.3,
                          r2y = "hello"))

  # Specified one and not the other
  expect_error(contour_plot(estimate = 2,
                            se = 0.5,
                            dof = 200,
                            r2d = 0.3))

  # Valid plots
  contour_plot(estimate = 2,
             se = 0.5,
             dof = 200,
             r2y = 0.1,
             r2d = 0.1)

  contour_plot(estimate = 2,
             se = 0.5,
             dof = 200,
             r2y = 0.1,
             r2d = 0.1,
             multipliers_d = c(1, 2, 3),
             multipliers_y = c(1, 2, 2.5))
})

test_that("bound_calculator misc test.", {
  expect_error(bound_calculator(0.1, 0.1, c(0.5, 0.8), 0.3))
  expect_error(bound_calculator(0.1, 0.1, 0.3, c(0.5, 0.8)))
  expect_error(bound_calculator(0.1, 0.1, -0.1, 0.3))
  expect_error(bound_calculator(0.1, 0.1, 0.3, -0.1))

  result_bc = bound_calculator(0.1, 0.1, 0.3, 0.3)
  expect_equal(result_bc$r2_dz, 0.033, tolerance=1e-3)
  expect_equal(result_bc$r2_yz, 0.041, tolerance=1e-3)
})

test_that("invalid confounder", {
  test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur,
                       benchmark = "female")

  expect_error(contour_plot(effect_model = test_obj$model_outcome,
             treatment_model = test_obj$model_treatment,
             treatment_covariate = "directlyharmed",
             benchmark_covariate = "test1"))
})
