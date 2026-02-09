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

test_that("contour plot for negative coefficients: fixest matches lm", {

  lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
                   herder_dar + pastvoted + hhsize_darfur + female + village,
                 data = darfur)

  feols.out  <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur)

  # age has a negative coefficient — verify t.threshold sign is handled correctly
  expect_true(coef(lm.out)["age"] < 0)

  # internally computed t.threshold should have sign(estimate) applied
  # verify by checking that lm and fixest produce same results for all sensitivity.of types
  for (s in c("estimate", "t-value", "lwr", "upr")) {
    lm_plot <- ovb_contour_plot(lm.out, treatment = "age", sensitivity.of = s)
    fe_plot <- ovb_contour_plot(feols.out, treatment = "age", sensitivity.of = s)
    expect_equal(lm_plot$value, fe_plot$value, tolerance = 1e-4,
                 info = paste("sensitivity.of =", s))
  }
})

test_that("contour plot alpha parameter is respected for fixest", {

  lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
                   herder_dar + pastvoted + hhsize_darfur + female + village,
                 data = darfur)

  feols.out  <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur)

  # with non-default alpha, lm and fixest should produce the same CI contour values
  for (s in c("lwr", "upr")) {
    lm_plot <- ovb_contour_plot(lm.out, treatment = "directlyharmed",
                                sensitivity.of = s, alpha = 0.01)
    fe_plot <- ovb_contour_plot(feols.out, treatment = "directlyharmed",
                                sensitivity.of = s, alpha = 0.01)
    expect_equal(lm_plot$value, fe_plot$value, tolerance = 1e-4,
                 info = paste("sensitivity.of =", s, ", alpha = 0.01"))
  }

  # verify alpha actually changes the result (alpha=0.01 != alpha=0.05)
  fe_01 <- ovb_contour_plot(feols.out, treatment = "directlyharmed",
                             sensitivity.of = "lwr", alpha = 0.01)
  fe_05 <- ovb_contour_plot(feols.out, treatment = "directlyharmed",
                             sensitivity.of = "lwr", alpha = 0.05)
  expect_false(isTRUE(all.equal(fe_01$value, fe_05$value, tolerance = 1e-4)))
})
