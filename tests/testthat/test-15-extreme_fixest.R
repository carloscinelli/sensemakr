context("Tests for Extreme Plot")



# fixest is Suggests-only: R CMD check runs the test suite with a library
# containing only the test framework, so this file must not error there.
if (requireNamespace("fixest", quietly = TRUE)) {

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

}
