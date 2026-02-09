context("Coverage gap tests")

# =========================================================================
# Setup: shared data and models used across tests
# =========================================================================

data("darfur")

lm.out <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
               herder_dar + pastvoted + hhsize_darfur + female + village,
             data = darfur)

feols.out <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                             herder_dar + pastvoted + hhsize_darfur + female + village,
                           data = darfur)

# =========================================================================
# bias_functions.R coverage gaps
# =========================================================================

# --- adjusted_ci.lm (lines 249-255) ---
test_that("adjusted_ci.lm works and matches numeric", {
  ci_lm <- adjusted_ci(lm.out, treatment = "directlyharmed",
                        r2dz.x = 0.05, r2yz.dx = 0.05)
  md <- model_helper(lm.out, covariates = "directlyharmed")
  ci_num <- adjusted_ci(estimate = md$estimate, se = md$se, dof = md$dof,
                         r2dz.x = 0.05, r2yz.dx = 0.05)
  expect_equal(unname(drop(ci_lm)), unname(drop(ci_num)), tolerance = 1e-10)
})

# --- adjusted_ci.fixest (lines 264-279) ---
test_that("adjusted_ci.fixest works and matches lm", {
  ci_fe <- adjusted_ci(feols.out, treatment = "directlyharmed",
                        r2dz.x = 0.05, r2yz.dx = 0.05)
  ci_lm <- adjusted_ci(lm.out, treatment = "directlyharmed",
                        r2dz.x = 0.05, r2yz.dx = 0.05)
  expect_equal(unname(ci_fe), unname(ci_lm), tolerance = 1e-4)

  # also test with non-default alpha and reduce
  ci_fe2 <- adjusted_ci(feols.out, treatment = "directlyharmed",
                         r2dz.x = 0.05, r2yz.dx = 0.05,
                         alpha = 0.01, reduce = FALSE)
  ci_lm2 <- adjusted_ci(lm.out, treatment = "directlyharmed",
                          r2dz.x = 0.05, r2yz.dx = 0.05,
                          alpha = 0.01, reduce = FALSE)
  expect_equal(unname(ci_fe2), unname(ci_lm2), tolerance = 1e-4)

  # test "lwr" and "upr" which selectors
  ci_fe_lwr <- adjusted_ci(feols.out, treatment = "directlyharmed",
                             r2dz.x = 0.05, r2yz.dx = 0.05, which = "lwr")
  ci_fe_upr <- adjusted_ci(feols.out, treatment = "directlyharmed",
                             r2dz.x = 0.05, r2yz.dx = 0.05, which = "upr")
  expect_true(ci_fe_lwr < ci_fe_upr)
})

# --- adjusted_ci.numeric error for non-numeric estimate (line 297) ---
test_that("adjusted_ci.numeric errors on non-numeric estimate", {
  # Use .numeric directly since S3 dispatch won't reach it for "character"
  expect_error(adjusted_ci.numeric(estimate = "hello", se = 0.5, dof = 200,
                                    r2dz.x = 0.05, r2yz.dx = 0.05),
               "Estimate provided must be a single number")
  # Also test vector estimate
  expect_error(adjusted_ci.numeric(estimate = c(1, 2), se = 0.5, dof = 200,
                                    r2dz.x = 0.05, r2yz.dx = 0.05),
               "Estimate provided must be a single number")
})

# --- adjusted_ci.numeric which = "both" return (line 310) ---
test_that("adjusted_ci.numeric returns matrix when which = 'both'", {
  ci <- adjusted_ci(estimate = 0.1, se = 0.02, dof = 783,
                     r2dz.x = 0.05, r2yz.dx = 0.05, which = "both")
  expect_true(is.matrix(ci))
  expect_equal(ncol(ci), 2)
  expect_equal(colnames(ci), c("lwr", "upr"))
})

# --- adjusted_se.fixest with non-iid vcov message (lines 220-221) ---
test_that("adjusted_se.fixest message for non-iid vcov", {
  feols_hc1 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur, vcov = "hetero")
  expect_message(
    adjusted_se(feols_hc1, treatment = "directlyharmed",
                r2dz.x = 0.05, r2yz.dx = 0.05),
    "iid"
  )
  # also test with message = FALSE (no message)
  expect_silent(
    adjusted_se(feols_hc1, treatment = "directlyharmed",
                r2dz.x = 0.05, r2yz.dx = 0.05, message = FALSE)
  )
})

# --- adjusted_t.fixest with non-iid vcov message (lines 346-347) ---
test_that("adjusted_t.fixest message for non-iid vcov", {
  feols_hc1 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur, vcov = "hetero")
  expect_message(
    adjusted_t(feols_hc1, treatment = "directlyharmed",
               r2dz.x = 0.05, r2yz.dx = 0.05),
    "iid"
  )
})

# --- adjusted_ci.fixest with non-iid vcov message (lines 264-268) ---
test_that("adjusted_ci.fixest message for non-iid vcov", {
  feols_hc1 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur, vcov = "hetero")
  expect_message(
    adjusted_ci(feols_hc1, treatment = "directlyharmed",
                r2dz.x = 0.05, r2yz.dx = 0.05),
    "iid"
  )
})

# --- print.t_stats (lines 381-386) ---
test_that("print.t_stats works", {
  at <- adjusted_t(estimate = 0.1, se = 0.02, dof = 783,
                    r2dz.x = 0.05, r2yz.dx = 0.05, h0 = 0.02)
  expect_output(print(at), "H0:tau =")
})

# --- relative_bias.fixest (lines 497-503) ---
test_that("relative_bias.fixest works and matches lm", {
  rb_fe <- relative_bias(feols.out, treatment = "directlyharmed",
                          r2dz.x = 0.05, r2yz.dx = 0.05)
  rb_lm <- relative_bias(lm.out, treatment = "directlyharmed",
                           r2dz.x = 0.05, r2yz.dx = 0.05)
  expect_equal(unname(rb_fe), unname(rb_lm), tolerance = 1e-4)
})


# =========================================================================
# sensitivity_stats.R coverage gaps
# =========================================================================

# --- rv() shorthand wrapper (line 76) ---
test_that("rv() shorthand works", {
  rv_val <- rv(lm.out, covariates = "directlyharmed")
  rv_full <- robustness_value(lm.out, covariates = "directlyharmed")
  expect_equal(rv_val, rv_full)
})

# --- xrv() shorthand wrapper (line 239) ---
test_that("xrv() shorthand works", {
  xrv_val <- xrv(lm.out, covariates = "directlyharmed")
  xrv_full <- extreme_robustness_value(lm.out, covariates = "directlyharmed")
  expect_equal(xrv_val, xrv_full)
})

# --- extreme_robustness_value.lm (lines 251-263) ---
test_that("extreme_robustness_value.lm works", {
  xrv_lm <- extreme_robustness_value(lm.out, covariates = "directlyharmed")
  expect_true(is.numeric(xrv_lm))
  expect_true(xrv_lm > 0)

  # match numeric version
  md <- model_helper(lm.out, covariates = "directlyharmed")
  xrv_num <- extreme_robustness_value(t_statistic = md$t_statistics,
                                       dof = md$dof)
  expect_equal(unname(xrv_lm), unname(xrv_num), tolerance = 1e-10)
})

# --- extreme_robustness_value.fixest (lines 278-294) ---
test_that("extreme_robustness_value.fixest works and matches lm", {
  xrv_fe <- extreme_robustness_value(feols.out, covariates = "directlyharmed")
  xrv_lm <- extreme_robustness_value(lm.out, covariates = "directlyharmed")
  expect_equal(unname(xrv_fe), unname(xrv_lm), tolerance = 1e-4)

  # test with non-default params
  xrv_fe2 <- extreme_robustness_value(feols.out, covariates = "directlyharmed",
                                       q = 0.5, alpha = 0.01)
  expect_true(is.numeric(xrv_fe2))
})

# --- extreme_robustness_value.fixest with non-iid vcov message ---
test_that("extreme_robustness_value.fixest message for non-iid vcov", {
  feols_hc1 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                                herder_dar + pastvoted + hhsize_darfur + female + village,
                              data = darfur, vcov = "hetero")
  expect_message(
    extreme_robustness_value(feols_hc1, covariates = "directlyharmed",
                              alpha = 0.05),
    "iid"
  )
  # no message when alpha = 1
  expect_silent(
    extreme_robustness_value(feols_hc1, covariates = "directlyharmed",
                              alpha = 1, message = TRUE)
  )
})

# --- extreme_robustness_value.default (lines 301-304) ---
test_that("extreme_robustness_value.default errors on unsupported type", {
  expect_error(extreme_robustness_value(model = "not_a_model"),
               "must be passed either")
})

# --- robustness_value.default ---
test_that("robustness_value.default errors on unsupported type", {
  expect_error(robustness_value(model = list(a = 1)),
               "must be passed either")
})

# --- print.rv (lines 351-360) ---
test_that("print.rv works", {
  rv_val <- robustness_value(t_statistic = 4.18, dof = 783,
                              q = 0.5, alpha = 0.05, invert = FALSE)
  expect_output(print(rv_val), "q =")
  expect_output(print(rv_val), "alpha =")
  expect_output(print(rv_val), "invert =")

  # also test invert = TRUE
  rv_inv <- robustness_value(t_statistic = 4.18, dof = 783,
                              q = 1, alpha = 0.05, invert = TRUE)
  expect_output(print(rv_inv), "invert = TRUE")
})

# Note: partial_f2.default and partial_f.default (lines 507-510, 544-549)
# are not registered in NAMESPACE, so S3 dispatch does not reach them.
# partial_r2.default IS registered and tested via robustness_value/extreme_robustness_value.
# These .default methods are defensive dead code.

# --- partial_r2.default ---
test_that("partial_r2.default errors on unsupported type", {
  # partial_r2.default IS registered in NAMESPACE
  expect_error(partial_r2(model = data.frame(a = 1)),
               "must be passed either")
})

# --- partial_f.fixest and partial_f.lm (lines 527, 533) ---
test_that("partial_f.fixest and partial_f.lm work", {
  pf_lm <- partial_f(lm.out, covariates = "directlyharmed")
  pf_fe <- partial_f(feols.out, covariates = "directlyharmed")
  expect_equal(unname(pf_fe), unname(pf_lm), tolerance = 1e-4)
  expect_true(pf_lm > 0)
})

# --- check_r (lines 827-828) ---
test_that("check_r errors on invalid input", {
  check_r <- sensemakr:::check_r
  expect_error(check_r(r = -0.1), "must be between 0 and 1")
  expect_error(check_r(r = 1.1), "must be between 0 and 1")
  expect_error(check_r(r = "hello"), "must be between 0 and 1")
  expect_error(check_r(r = c(0.1, 0.2)), "must be between 0 and 1")
})

# Note: error_if_no_dof.fixest (lines 944-945) is practically unreachable
# because fixest::degrees_freedom never returns 0 even for saturated models.
# This is defensive code.


# =========================================================================
# ovb_plots.R coverage gaps
# =========================================================================

# --- contour plot with sensitivity.of = "lwr" and "upr" with bound (lines 602-609) ---
test_that("contour plot sensitivity.of lwr/upr with bounds", {
  # lwr with benchmarks (hits bound_value adjusted_ci branch)
  plt_lwr <- ovb_contour_plot(lm.out, treatment = "directlyharmed",
                               benchmark_covariates = "female",
                               kd = 1, ky = 1,
                               sensitivity.of = "lwr")
  expect_true(!is.null(plt_lwr$value))

  # upr with benchmarks
  plt_upr <- ovb_contour_plot(lm.out, treatment = "directlyharmed",
                               benchmark_covariates = "female",
                               kd = 1, ky = 1,
                               sensitivity.of = "upr")
  expect_true(!is.null(plt_upr$value))

  # with direct r2dz.x/r2yz.dx (numeric path)
  plt_lwr2 <- ovb_contour_plot(estimate = 0.1, se = 0.02, dof = 783,
                                r2dz.x = 0.05, r2yz.dx = 0.05,
                                sensitivity.of = "lwr",
                                bound_label = "test")
  expect_true(!is.null(plt_lwr2$value))
})

# --- contour plot formula method with feols (line 416 fixest requireNamespace)
# Note: we can't easily test the "fixest not installed" branch,
# but we can test the formula+feols path to exercise the non-error branch
test_that("contour plot formula method with feols", {
  plt <- ovb_contour_plot(formula = peacefactor ~ directlyharmed + age +
                            farmer_dar + herder_dar + pastvoted +
                            hhsize_darfur + female + village,
                          data = darfur,
                          method = "feols",
                          treatment = "directlyharmed")
  expect_true(!is.null(plt$value))
})

# --- extreme plot formula method with feols (lines 1223-1228) ---
test_that("extreme plot formula method with feols", {
  plt <- ovb_extreme_plot(formula = peacefactor ~ directlyharmed + age +
                            farmer_dar + herder_dar + pastvoted +
                            hhsize_darfur + female + village,
                          data = darfur,
                          method = "feols",
                          treatment = "directlyharmed")
  expect_true(!is.null(plt))
})


# =========================================================================
# print.R coverage gaps
# =========================================================================

# --- ovb_minimal_reporting without benchmarks (lines 341-345, 444-448) ---
test_that("ovb_minimal_reporting without benchmarks works", {
  # sensemakr without benchmark covariates
  s_no_bench <- sensemakr(lm.out, treatment = "directlyharmed")

  # html format without benchmarks
  out_html <- ovb_minimal_reporting(s_no_bench, format = "html", verbose = FALSE)
  expect_true(is.character(out_html))
  expect_true(grepl("Note: df =", out_html))
  # should NOT contain "Bound" since no benchmarks
  expect_false(grepl("Bound", out_html))

  # pure_html format without benchmarks
  out_pure <- ovb_minimal_reporting(s_no_bench, format = "pure_html", verbose = FALSE)
  expect_true(is.character(out_pure))
  expect_false(grepl("Bound", out_pure))
})

# --- ovb_minimal_reporting verbose = TRUE (lines 357, 460) ---
test_that("ovb_minimal_reporting verbose = TRUE prints output", {
  s_out <- sensemakr(lm.out, treatment = "directlyharmed",
                      benchmark_covariates = "female", kd = 1:3)

  # html verbose
  expect_output(
    ovb_minimal_reporting(s_out, format = "html", verbose = TRUE),
    "table"
  )

  # pure_html verbose
  expect_output(
    ovb_minimal_reporting(s_out, format = "pure_html", verbose = TRUE),
    "table"
  )

  # without benchmarks, verbose
  s_no_bench <- sensemakr(lm.out, treatment = "directlyharmed")
  expect_output(
    ovb_minimal_reporting(s_no_bench, format = "html", verbose = TRUE),
    "table"
  )
  expect_output(
    ovb_minimal_reporting(s_no_bench, format = "pure_html", verbose = TRUE),
    "table"
  )
})


# =========================================================================
# sensemakr.R coverage gaps
# =========================================================================

# --- sensemakr.fixest non-feols method check (line 292) ---
# This is hard to trigger since fixest models always have method = "feols"
# for feols() calls. We test by manually creating a mock.
test_that("sensemakr.fixest errors for non-feols fixest methods", {
  # fixest::fenegbin would have method = "fenegbin"
  # We can test this by modifying the model object
  mock_model <- feols.out
  mock_model$method <- "fenegbin"
  expect_error(
    sensemakr(mock_model, treatment = "directlyharmed"),
    "only implemented for feols"
  )
})

# --- sensemakr.formula feols path (line 408 requireNamespace) ---
# Can't test "fixest not installed", but exercise the feols formula path
test_that("sensemakr.formula with feols method works", {
  s_formula <- sensemakr(formula = peacefactor ~ directlyharmed + age +
                           farmer_dar + herder_dar + pastvoted +
                           hhsize_darfur + female + village,
                         data = darfur,
                         method = "feols",
                         treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)
  s_feols <- sensemakr(feols.out, treatment = "directlyharmed",
                        benchmark_covariates = "female",
                        kd = 1:3)
  expect_equal(s_formula$sensitivity_stats, s_feols$sensitivity_stats,
               tolerance = 1e-4)
})


# =========================================================================
# ovb_bounds.R coverage gaps
# =========================================================================

# --- impossible kd value error (line 298) ---
test_that("ovb_bounds errors on impossible kd value (r2dz.x >= 1)", {
  # Line 298: r2dz.x >= 1 fires for extreme kd values
  # For female as benchmark, r2dxj.x ≈ 0.009, need kd >= 110
  expect_error(
    ovb_bounds(lm.out, treatment = "directlyharmed",
               benchmark_covariates = "female",
               kd = 110),
    "Implied bound on r2dz.x >= 1"
  )
})

# Note: line 306 (r2zxj.xd >= 1 check) is mathematically unreachable:
# If r2dz.x < 1 (line 298 passes), then r2zxj.xd = r2dxj.x * r2dz.x / (1 - kd*r2dxj.x)
# is necessarily < 1. This is defensive dead code.
