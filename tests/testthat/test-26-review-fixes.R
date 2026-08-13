context("Regression tests for reviewed defects")

data("darfur")

model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)

null_device <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

# =========================================================================
# Contour plots
# =========================================================================

test_that("the critical threshold follows the sign of the estimate", {
  # every method must agree; .numeric used to omit sign(estimate)
  neg <- null_device(ovb_contour_plot(estimate = -0.0973, se = 0.0233, dof = 783,
                                      sensitivity.of = "t-value"))
  pos <- null_device(ovb_contour_plot(estimate = 0.0973, se = 0.0233, dof = 783,
                                      sensitivity.of = "t-value"))
  expect_equal(neg$threshold, -pos$threshold)
  expect_lt(neg$threshold, 0)
})

test_that("alpha reaches the confidence limit surface", {
  s20 <- sensemakr(model, treatment = "directlyharmed", alpha = 0.20)
  s05 <- sensemakr(model, treatment = "directlyharmed", alpha = 0.05)
  p20 <- null_device(plot(s20, sensitivity.of = "lwr"))
  p05 <- null_device(plot(s05, sensitivity.of = "lwr"))
  # a wider alpha gives a higher lower limit, so the surfaces must differ
  expect_false(isTRUE(all.equal(p20$value, p05$value)))
  expect_gt(min(p20$value), min(p05$value))
})

test_that("the extreme plot axis covers the strongest bound", {
  b <- ovb_bounds(model, treatment = "directlyharmed",
                  benchmark_covariates = c("female", "age"), kd = c(1, 30))
  md <- model_helper(model, covariates = "directlyharmed")

  p <- null_device(
    suppressWarnings(ovb_extreme_plot(estimate = md$estimate, se = md$se,
                                      dof = md$dof, r2dz.x = b$r2dz.x)))
  expect_gte(max(p[[1]]$r2dz.x), max(b$r2dz.x))

  # the lm method never saw the bounds at all: its limit was a constant
  p_lm <- null_device(
    suppressWarnings(ovb_extreme_plot(model, treatment = "directlyharmed",
                                      benchmark_covariates = c("female", "age"),
                                      kd = c(1, 30))))
  expect_gte(max(p_lm[[1]]$r2dz.x), max(b$r2dz.x))
})

test_that("the formula method accepts a manual bound", {
  expect_silent(null_device(
    ovb_contour_plot(formula = peacefactor ~ directlyharmed + age + female,
                     data = darfur, method = "lm", treatment = "directlyharmed",
                     r2dz.x = 0.1, r2yz.dx = 0.1)))
})

test_that("bounds can be added after a confidence limit plot", {
  expect_silent(null_device({
    ovb_contour_plot(model, treatment = "directlyharmed", sensitivity.of = "lwr")
    add_bound_to_contour(model, treatment = "directlyharmed",
                         benchmark_covariates = "female", kd = 1)
  }))
})

test_that("sensitivity.of can be overridden in add_bound_to_contour", {
  expect_silent(null_device({
    ovb_contour_plot(model, treatment = "directlyharmed")
    add_bound_to_contour(model, treatment = "directlyharmed",
                         benchmark_covariates = "female", kd = 1,
                         sensitivity.of = "t-value")
  }))
})

# =========================================================================
# Models that are not OLS
# =========================================================================

test_that("models inheriting from lm are refused with a useful message", {
  g <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  expect_true(inherits(g, "lm"))

  expect_error(sensitivity_stats(g, treatment = "wt"), "defined for OLS")
  expect_error(sensemakr(g, treatment = "wt"), "defined for OLS")
  expect_error(partial_r2(g, covariates = "wt"), "defined for OLS")
  # this one used to return a number for a logistic fit
  expect_error(group_partial_r2(g, covariates = "wt"), "defined for OLS")

  expect_error(partial_r2(aov(mpg ~ wt, data = mtcars)), "defined for OLS")
})

# =========================================================================
# adjusted_critical_value: Theorem 3
# =========================================================================

test_that("the adjusted critical value equals |t| at the robustness value", {
  # the robustness value is by definition the strength at which significance is
  # exactly lost, so the two must coincide there
  for (tt in c(4, 8, 20)) {
    for (d in c(5, 10, 30)) {
      rv <- as.numeric(robustness_value(t_statistic = tt, dof = d, alpha = 0.05))
      acv <- as.numeric(adjusted_critical_value(r2dz.x = rv, r2yz.dx = rv,
                                                dof = d, alpha = 0.05, max = TRUE))
      expect_equal(acv, tt, tolerance = 1e-8,
                   info = paste("t =", tt, "dof =", d))
    }
  }
})

test_that("adjusted_critical_value recycles either argument", {
  many <- adjusted_critical_value(r2dz.x = c(0.001, 0.002, 0.003),
                                  r2yz.dx = 0.9, dof = 100)
  expect_length(many, 3)
  expect_false(any(is.na(many)))

  flipped <- adjusted_critical_value(r2dz.x = 0.9,
                                     r2yz.dx = c(0.001, 0.002, 0.003), dof = 100)
  expect_length(flipped, 3)
  expect_false(any(is.na(flipped)))
})

# =========================================================================
# Smaller defects
# =========================================================================

test_that("degrees of freedom below 2 are rejected", {
  tiny <- lm(y ~ x, data = data.frame(y = c(1, 2, 3), x = c(1, 2, 4)))
  expect_error(sensitivity_stats(tiny, treatment = "x"), "greater than or equal to 2")
  expect_error(robustness_value(t_statistic = 2, dof = 1), "greater than or equal to 2")
})

test_that("a zero estimate is still moved by a confounder", {
  b <- as.numeric(bias(se = 3, dof = 100, r2dz.x = 0.3, r2yz.dx = 0.4))
  adj <- as.numeric(adjusted_estimate(estimate = 0, se = 3, dof = 100,
                                      r2dz.x = 0.3, r2yz.dx = 0.4))
  expect_equal(adj, -b)
})

test_that("bounds keep their class when manual and benchmark are combined", {
  both <- sensemakr(model, treatment = "directlyharmed",
                    benchmark_covariates = "female", kd = 1,
                    r2dz.x = 0.1, r2yz.dx = 0.1)
  expect_s3_class(both$bounds, "ovb_bounds")
  expect_silent(null_device({
    ovb_contour_plot(model, treatment = "directlyharmed")
    add_bound_to_contour(both$bounds)
  }))
})

test_that("the registered default methods are reachable", {
  expect_error(partial_f("not a model"), "must be passed either")
  expect_error(partial_f2("not a model"), "must be passed either")
  expect_error(partial_r2(data.frame(a = 1)), "must be passed either")
})

test_that("the treatment name prints without stray spaces", {
  out <- capture.output(print(sensemakr(model, treatment = "directlyharmed")))
  expect_true(any(grepl("'directlyharmed'", out, fixed = TRUE)))
  expect_false(any(grepl("' directlyharmed '", out, fixed = TRUE)))
})
