context("Tests for Extreme Plot")



test_that("extreme plot", {

  feols.out  <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                  herder_dar + pastvoted + hhsize_darfur + female + village, data = darfur)

  expect_invisible(ovb_extreme_plot(estimate = 2, se = 0.5, dof = 200))

  expect_invisible(ovb_extreme_plot(estimate = 2, se = 0.5, dof = 200, r2d = 0.2))

  expect_invisible(ovb_extreme_plot(feols.out, treatment = "directlyharmed"))

  expect_invisible(ovb_extreme_plot(feols.out, treatment = "directlyharmed",
                                    benchmark_covariates = "female"))

  expect_invisible(ovb_extreme_plot(feols.out, treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    kd = 1:3))

  expect_invisible(ovb_extreme_plot(formula = peacefactor ~ directlyharmed + age + farmer_dar +
                                      herder_dar + pastvoted + hhsize_darfur + female + village,
                                    data = darfur,
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    kd = 1:3))

})
