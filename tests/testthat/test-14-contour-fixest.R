context("Tests for Contour Plots fixest")


test_that("contour plot tests", {

  feols.out  <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
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
  expect_error(ovb_contour_plot(feols.out,
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = c(1, 2, 3), ky = c(1, 3)))

  expect_error(ovb_contour_plot(feols.out,
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = -1))

  expect_error(ovb_contour_plot(feols.out,
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
                                r2dz.x = .2,
                                r2yz.dx = c(.1,.2,.3)))

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
  expect_error(ovb_contour_plot(model = feols.out,
                                treatment = "directlyharmed",
                                benchmark_covariate = c("test1", "test2")))
  # invalid treatment
  expect_error(ovb_contour_plot(model = feols.out,
                                treatment = "directly harmed",
                                benchmark_covariate = "test1"))

  # Valid plots
  expect_invisible(ovb_contour_plot(estimate = 2,
                                    se = 0.5,
                                    dof = 200,
                                    r2yz.dx = 0.1,
                                    r2dz.x = 0.1,
                                    bound_label = ""))

  expect_warning(ovb_contour_plot(estimate = 2,
                                  se = 0.5,
                                  dof = 200,
                                  r2yz.dx = 0.1,
                                  r2dz.x = 0.1,
                                  bound_label = "", lim.y = 1.1))

  expect_warning(ovb_contour_plot(estimate = 2,
                                  se = 0.5,
                                  dof = 200,
                                  r2yz.dx = 0.1,
                                  r2dz.x = 0.1,
                                  bound_label = "", lim.y = -.1))

  expect_invisible(ovb_contour_plot(estimate = 2,
                                    se = 0.5,
                                    dof = 200,
                                    r2yz.dx = 0.1,
                                    r2dz.x = 0.1,
                                    bound_label = "my bound"))
  # feols
  expect_invisible(ovb_contour_plot(feols.out,
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    kd = 1:3, ky = 1:3))

  # formula
  expect_invisible(ovb_contour_plot(formula = peacefactor ~ directlyharmed + age + farmer_dar +
                                      herder_dar + pastvoted + hhsize_darfur + female + village, data = darfur,
                                    method = "feols",
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    kd = 1:3, ky = 1:3))
})
