context("test-contour.R")


test_that("contour plot tests", {

  lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
                  herder_dar + pastvoted + hhsize_darfur + female + village,
                data = darfur)

  # Missing everything
  expect_error(ovb_contour_plot())

  # Nonnumeric estimate
  expect_error(ovb_contour_plot(estimate = "hello"))

  # Missing estimate
  expect_error(ovb_contour_plot(estimate = 2))

  # Missing DOF
  expect_error(ovb_contour_plot(estimate = 2, se = 0.5))

  # Misspecified multipliers
  expect_error(ovb_contour_plot(lm.out,
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = c(1, 2, 3), ky = c(1, 3)))

  expect_error(ovb_contour_plot(lm.out,
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = -1))

  expect_error(ovb_contour_plot(lm.out,
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = "hello"))

  # Misspecified r2dz.x
  expect_error(ovb_contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2dz.x = -1,
                          r2yz.dx = 0.3))

  expect_error(ovb_contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2dz.x = "hello",
                          r2yz.dx = 0.3))

  expect_error(ovb_contour_plot(estimate = 2,
                                se = 0.5,
                                dof = 200,
                                r2dz.x = c(0.5, 0.8),
                                r2yz.dx = 0.3))

  # Misspecified r2yz.dx
  expect_error(ovb_contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2dz.x = 0.3,
                          r2yz.dx = -1))

  expect_error(ovb_contour_plot(estimate = 2,
                          se = 0.5,
                          dof = 200,
                          r2dz.x = 0.3,
                          r2yz.dx = "hello"))


  # invalid confounder
  expect_error(ovb_contour_plot(model = lm.out,
                                treatment = "directlyharmed",
                                benchmark_covariate = c("test1", "test2")))
  # invalid treatment
  expect_error(ovb_contour_plot(model = lm.out,
                                treatment = "directly harmed",
                                benchmark_covariate = "test1"))

  # Valid plots
  ovb_contour_plot(estimate = 2,
                   se = 0.5,
                   dof = 200,
                   r2yz.dx = 0.1,
                   r2dz.x = 0.1,
                   bound_label = "")

  ovb_contour_plot(estimate = 2,
                   se = 0.5,
                   dof = 200,
                   r2yz.dx = 0.1,
                   r2dz.x = 0.1,
                   bound_label = "my bound")
  # lm
  ovb_contour_plot(lm.out,
                   treatment = "directlyharmed",
                   benchmark_covariates = "female",
                   kd = 1:3, ky = 1:3)

  # formula
  ovb_contour_plot(peacefactor ~ directlyharmed + age + farmer_dar +
                     herder_dar + pastvoted + hhsize_darfur + female + village, data = darfur,
                   treatment = "directlyharmed",
                   benchmark_covariates = "female",
                   kd = 1:3, ky = 1:3)
})

# test_that("bound_calculator misc test.", {
#   expect_error(bound_calculator(0.1, 0.1, c(0.5, 0.8), 0.3))
#   expect_error(bound_calculator(0.1, 0.1, 0.3, c(0.5, 0.8)))
#   expect_error(bound_calculator(0.1, 0.1, -0.1, 0.3))
#   expect_error(bound_calculator(0.1, 0.1, 0.3, -0.1))
#
#   result_bc = bound_calculator(0.1, 0.1, 0.3, 0.3)
#   expect_equal(result_bc$r2_dz, 0.033, tolerance=1e-3)
#   expect_equal(result_bc$r2_yz, 0.041, tolerance=1e-3)
# })
#
