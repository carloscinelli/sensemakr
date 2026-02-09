context("Edge case tests: cross-method consistency, negative coefficients, non-default parameters")

# =========================================================================
# Setup: shared data and models
# =========================================================================

data("darfur")

lm.out <- lm(peacefactor ~ directlyharmed + age + farmer_dar +
               herder_dar + pastvoted + hhsize_darfur + female + village,
             data = darfur)

feols.out <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar +
                             herder_dar + pastvoted + hhsize_darfur + female + village,
                           data = darfur)

# Extract numeric values for directlyharmed (positive coef)
md_pos <- model_helper(lm.out, covariates = "directlyharmed")

# Extract numeric values for age (negative coef)
md_neg <- model_helper(lm.out, covariates = "age")

# Helper: get the correct r2dxj.x from the treatment model
# (partial R2 of benchmark in the regression of treatment on other covariates)
get_r2dxj.x <- function(lm_model, treatment, benchmark) {
  mf <- model.frame(lm_model)
  covariates <- setdiff(names(mf)[-1], treatment)
  treat_formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))
  treat_model <- lm(treat_formula, data = mf)
  partial_r2(treat_model, covariates = benchmark)
}


# =========================================================================
# 1. Cross-method consistency: lm vs fixest vs numeric
# =========================================================================

# --- adjusted_estimate: lm vs fixest vs numeric ---
test_that("adjusted_estimate: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    for (red in c(TRUE, FALSE)) {
      ae_lm  <- adjusted_estimate(lm.out, treatment = treat,
                                   r2dz.x = 0.05, r2yz.dx = 0.05, reduce = red)
      ae_fe  <- adjusted_estimate(feols.out, treatment = treat,
                                   r2dz.x = 0.05, r2yz.dx = 0.05, reduce = red)
      ae_num <- adjusted_estimate(estimate = md$estimate, se = md$se, dof = md$dof,
                                   r2dz.x = 0.05, r2yz.dx = 0.05, reduce = red)
      expect_equal(unname(ae_lm), ae_num, tolerance = 1e-10,
                   label = paste("adjusted_estimate", treat, "reduce =", red))
      expect_equal(unname(ae_fe), ae_num, tolerance = 1e-4,
                   label = paste("adjusted_estimate fe", treat, "reduce =", red))
    }
  }
})

# --- adjusted_se: lm vs fixest vs numeric ---
test_that("adjusted_se: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    ase_lm  <- adjusted_se(lm.out, treatment = treat,
                            r2dz.x = 0.05, r2yz.dx = 0.05)
    ase_fe  <- adjusted_se(feols.out, treatment = treat,
                            r2dz.x = 0.05, r2yz.dx = 0.05)
    ase_num <- adjusted_se(se = md$se, dof = md$dof,
                            r2dz.x = 0.05, r2yz.dx = 0.05)
    expect_equal(unname(ase_lm), ase_num, tolerance = 1e-10,
                 label = paste("adjusted_se", treat))
    expect_equal(unname(ase_fe), ase_num, tolerance = 1e-4,
                 label = paste("adjusted_se fe", treat))
  }
})

# --- adjusted_t: lm vs fixest vs numeric ---
test_that("adjusted_t: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    for (red in c(TRUE, FALSE)) {
      for (h0_val in c(0, 0.01)) {
        at_lm  <- adjusted_t(lm.out, treatment = treat,
                              r2dz.x = 0.05, r2yz.dx = 0.05,
                              reduce = red, h0 = h0_val)
        at_fe  <- adjusted_t(feols.out, treatment = treat,
                              r2dz.x = 0.05, r2yz.dx = 0.05,
                              reduce = red, h0 = h0_val)
        at_num <- adjusted_t(estimate = md$estimate, se = md$se, dof = md$dof,
                              r2dz.x = 0.05, r2yz.dx = 0.05,
                              reduce = red, h0 = h0_val)
        expect_equal(as.numeric(at_lm), as.numeric(at_num), tolerance = 1e-10,
                     label = paste("adjusted_t", treat, "reduce =", red, "h0 =", h0_val))
        expect_equal(as.numeric(at_fe), as.numeric(at_num), tolerance = 1e-4,
                     label = paste("adjusted_t fe", treat, "reduce =", red, "h0 =", h0_val))
      }
    }
  }
})

# --- adjusted_ci: lm vs fixest vs numeric ---
test_that("adjusted_ci: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    for (red in c(TRUE, FALSE)) {
      for (alpha_val in c(0.05, 0.01)) {
        for (w in c("both", "lwr", "upr")) {
          ci_lm  <- adjusted_ci(lm.out, treatment = treat,
                                 r2dz.x = 0.05, r2yz.dx = 0.05,
                                 reduce = red, alpha = alpha_val, which = w)
          ci_fe  <- adjusted_ci(feols.out, treatment = treat,
                                 r2dz.x = 0.05, r2yz.dx = 0.05,
                                 reduce = red, alpha = alpha_val, which = w)
          ci_num <- adjusted_ci(estimate = md$estimate, se = md$se, dof = md$dof,
                                 r2dz.x = 0.05, r2yz.dx = 0.05,
                                 reduce = red, alpha = alpha_val, which = w)
          expect_equal(unname(drop(ci_lm)), unname(drop(ci_num)), tolerance = 1e-10,
                       label = paste("ci", treat, red, alpha_val, w))
          expect_equal(unname(drop(ci_fe)), unname(drop(ci_num)), tolerance = 1e-4,
                       label = paste("ci fe", treat, red, alpha_val, w))
        }
      }
    }
  }
})

# --- adjusted_partial_r2: lm vs fixest vs numeric ---
test_that("adjusted_partial_r2: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    for (red in c(TRUE, FALSE)) {
      for (h0_val in c(0, 0.01)) {
        ar2_lm  <- adjusted_partial_r2(lm.out, treatment = treat,
                                        r2dz.x = 0.05, r2yz.dx = 0.05,
                                        reduce = red, h0 = h0_val)
        ar2_fe  <- adjusted_partial_r2(feols.out, treatment = treat,
                                        r2dz.x = 0.05, r2yz.dx = 0.05,
                                        reduce = red, h0 = h0_val)
        ar2_num <- adjusted_partial_r2(estimate = md$estimate, se = md$se, dof = md$dof,
                                        r2dz.x = 0.05, r2yz.dx = 0.05,
                                        reduce = red, h0 = h0_val)
        expect_equal(unname(ar2_lm), ar2_num, tolerance = 1e-10,
                     label = paste("adj_r2", treat, red, h0_val))
        expect_equal(unname(ar2_fe), ar2_num, tolerance = 1e-4,
                     label = paste("adj_r2 fe", treat, red, h0_val))
      }
    }
  }
})

# --- bias: lm vs fixest vs numeric ---
test_that("bias: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    b_lm  <- bias(lm.out, treatment = treat,
                   r2dz.x = 0.05, r2yz.dx = 0.05)
    b_fe  <- bias(feols.out, treatment = treat,
                   r2dz.x = 0.05, r2yz.dx = 0.05)
    b_num <- bias(se = md$se, dof = md$dof,
                   r2dz.x = 0.05, r2yz.dx = 0.05)
    expect_equal(unname(b_lm), b_num, tolerance = 1e-10,
                 label = paste("bias", treat))
    expect_equal(unname(b_fe), b_num, tolerance = 1e-4,
                 label = paste("bias fe", treat))
  }
})

# --- relative_bias: lm vs fixest vs numeric ---
test_that("relative_bias: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    rb_lm  <- relative_bias(lm.out, treatment = treat,
                              r2dz.x = 0.05, r2yz.dx = 0.05)
    rb_fe  <- relative_bias(feols.out, treatment = treat,
                              r2dz.x = 0.05, r2yz.dx = 0.05)
    rb_num <- relative_bias(estimate = md$estimate, se = md$se, dof = md$dof,
                              r2dz.x = 0.05, r2yz.dx = 0.05)
    expect_equal(unname(rb_lm), rb_num, tolerance = 1e-10,
                 label = paste("relative_bias", treat))
    expect_equal(unname(rb_fe), rb_num, tolerance = 1e-4,
                 label = paste("relative_bias fe", treat))
  }
})

# --- sensitivity_stats: lm vs fixest vs numeric ---
test_that("sensitivity_stats: lm vs fixest vs numeric match", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    for (q_val in c(1, 0.5)) {
      for (red in c(TRUE, FALSE)) {
        ss_lm  <- sensitivity_stats(lm.out, treatment = treat,
                                     q = q_val, reduce = red)
        ss_fe  <- sensitivity_stats(feols.out, treatment = treat,
                                     q = q_val, reduce = red)
        ss_num <- sensitivity_stats(estimate = md$estimate, se = md$se, dof = md$dof,
                                     treatment = treat, q = q_val, reduce = red)
        # Compare key columns
        for (col in c("estimate", "se", "t_statistic", "r2yd.x", "rv_q", "rv_qa", "dof")) {
          expect_equal(ss_lm[[col]], ss_num[[col]], tolerance = 1e-10,
                       label = paste("ss", treat, q_val, red, col))
          expect_equal(ss_fe[[col]], ss_num[[col]], tolerance = 1e-4,
                       label = paste("ss fe", treat, q_val, red, col))
        }
      }
    }
  }
})

# --- ovb_bounds: lm vs fixest ---
test_that("ovb_bounds: lm vs fixest match", {
  bounds_lm <- ovb_bounds(lm.out, treatment = "directlyharmed",
                           benchmark_covariates = "female", kd = 1:3, ky = 1:3)
  bounds_fe <- ovb_bounds(feols.out, treatment = "directlyharmed",
                           benchmark_covariates = "female", kd = 1:3, ky = 1:3)
  expect_equal(bounds_lm$r2dz.x, bounds_fe$r2dz.x, tolerance = 1e-4)
  expect_equal(bounds_lm$r2yz.dx, bounds_fe$r2yz.dx, tolerance = 1e-4)
})


# =========================================================================
# 2. Negative coefficients
# =========================================================================

test_that("adjusted_estimate with negative coefficient and reduce/increase", {
  # age has negative coefficient (-0.002)
  ae_reduce   <- adjusted_estimate(lm.out, treatment = "age",
                                    r2dz.x = 0.05, r2yz.dx = 0.05, reduce = TRUE)
  ae_increase <- adjusted_estimate(lm.out, treatment = "age",
                                    r2dz.x = 0.05, r2yz.dx = 0.05, reduce = FALSE)
  # reduce should bring estimate closer to zero (less negative)
  expect_true(abs(ae_reduce) < abs(coef(lm.out)["age"]))
  # increase should make it more negative
  expect_true(abs(ae_increase) > abs(coef(lm.out)["age"]))
})

test_that("adjusted_t sign is correct for negative coefficients", {
  # For negative coefficient with reduce = TRUE, adjusted t should be
  # smaller in absolute value (closer to zero)
  md <- model_helper(lm.out, covariates = "age")
  original_t <- md$t_statistics
  at_reduce <- adjusted_t(lm.out, treatment = "age",
                           r2dz.x = 0.05, r2yz.dx = 0.05, reduce = TRUE)
  expect_true(abs(as.numeric(at_reduce)) < abs(original_t))
})

test_that("adjusted_ci with negative coefficient", {
  ci <- adjusted_ci(lm.out, treatment = "age",
                     r2dz.x = 0.05, r2yz.dx = 0.05)
  # For negative coef, lower bound should be more negative than upper
  expect_true(ci[, "lwr"] < ci[, "upr"])
  # Both should typically be negative for age
  expect_true(ci[, "upr"] < 0 || ci[, "lwr"] < 0)
})

test_that("extreme plot with negative coefficient: lm vs fixest", {
  plt_lm <- ovb_extreme_plot(lm.out, treatment = "age",
                              benchmark_covariates = "female", kd = 1)
  plt_fe <- ovb_extreme_plot(feols.out, treatment = "age",
                              benchmark_covariates = "female", kd = 1)
  expect_true(!is.null(plt_lm))
  expect_true(!is.null(plt_fe))
})

test_that("contour plot lwr/upr with negative coefficient: lm vs fixest", {
  for (s in c("lwr", "upr")) {
    plt_lm <- ovb_contour_plot(lm.out, treatment = "age",
                                sensitivity.of = s)
    plt_fe <- ovb_contour_plot(feols.out, treatment = "age",
                                sensitivity.of = s)
    expect_equal(plt_lm$value, plt_fe$value, tolerance = 1e-4,
                 label = paste("contour", s, "negative coef"))
  }
})

test_that("sensemakr with negative coefficient: lm vs fixest vs numeric", {
  md <- model_helper(lm.out, covariates = "age")
  s_lm  <- sensemakr(lm.out, treatment = "age",
                      benchmark_covariates = "female", kd = 1:3)
  s_fe  <- sensemakr(feols.out, treatment = "age",
                      benchmark_covariates = "female", kd = 1:3)
  s_num <- sensemakr(estimate = md$estimate, se = md$se, dof = md$dof,
                      treatment = "age",
                      benchmark_covariates = "female",
                      kd = 1:3,
                      r2dxj.x = get_r2dxj.x(lm.out, "age", "female"),
                      r2yxj.dx = partial_r2(lm.out, covariates = "female"))
  expect_equal(s_lm$sensitivity_stats, s_fe$sensitivity_stats, tolerance = 1e-4)
  expect_equal(s_lm$bounds$adjusted_estimate, s_fe$bounds$adjusted_estimate, tolerance = 1e-4)
  expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4)
  # Also check numeric matches lm
  expect_equal(s_lm$bounds$adjusted_estimate, s_num$bounds$adjusted_estimate, tolerance = 1e-5)
  expect_equal(s_lm$bounds$adjusted_t, s_num$bounds$adjusted_t, tolerance = 1e-5)
})


# =========================================================================
# 3. Non-default q values
# =========================================================================

test_that("sensemakr with q != 1: lm vs fixest vs numeric match", {
  for (q_val in c(0.5, 2)) {
    md <- model_helper(lm.out, covariates = "directlyharmed")
    s_lm  <- sensemakr(lm.out, treatment = "directlyharmed", q = q_val,
                        benchmark_covariates = "female", kd = 1:3)
    s_fe  <- sensemakr(feols.out, treatment = "directlyharmed", q = q_val,
                        benchmark_covariates = "female", kd = 1:3)
    s_num <- sensemakr(estimate = md$estimate, se = md$se, dof = md$dof,
                        treatment = "directlyharmed", q = q_val,
                        benchmark_covariates = "female",
                        kd = 1:3,
                        r2dxj.x = get_r2dxj.x(lm.out, "directlyharmed", "female"),
                        r2yxj.dx = partial_r2(lm.out, covariates = "female"))
    # bounds should match across all three
    expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4,
                 label = paste("q =", q_val, "lm vs fe adjusted_t"))
    expect_equal(s_lm$bounds$adjusted_t, s_num$bounds$adjusted_t, tolerance = 1e-5,
                 label = paste("q =", q_val, "lm vs num adjusted_t"))
    expect_equal(s_lm$bounds$adjusted_estimate, s_num$bounds$adjusted_estimate, tolerance = 1e-5,
                 label = paste("q =", q_val, "lm vs num adjusted_estimate"))
  }
})

test_that("q != 1 with negative coefficient: lm vs fixest vs numeric", {
  for (q_val in c(0.5, 2)) {
    md <- model_helper(lm.out, covariates = "age")
    s_lm  <- sensemakr(lm.out, treatment = "age", q = q_val,
                        benchmark_covariates = "female", kd = 1:3)
    s_fe  <- sensemakr(feols.out, treatment = "age", q = q_val,
                        benchmark_covariates = "female", kd = 1:3)
    s_num <- sensemakr(estimate = md$estimate, se = md$se, dof = md$dof,
                        treatment = "age", q = q_val,
                        benchmark_covariates = "female",
                        kd = 1:3,
                        r2dxj.x = get_r2dxj.x(lm.out, "age", "female"),
                        r2yxj.dx = partial_r2(lm.out, covariates = "female"))
    expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4,
                 label = paste("neg q =", q_val, "lm vs fe"))
    expect_equal(s_lm$bounds$adjusted_t, s_num$bounds$adjusted_t, tolerance = 1e-5,
                 label = paste("neg q =", q_val, "lm vs num"))
  }
})


# =========================================================================
# 4. Non-default alpha values
# =========================================================================

test_that("contour plot with alpha != 0.05: lm vs fixest match for all sensitivity.of", {
  for (s in c("estimate", "t-value", "lwr", "upr")) {
    for (alpha_val in c(0.01, 0.10)) {
      plt_lm <- ovb_contour_plot(lm.out, treatment = "directlyharmed",
                                  sensitivity.of = s, alpha = alpha_val)
      plt_fe <- ovb_contour_plot(feols.out, treatment = "directlyharmed",
                                  sensitivity.of = s, alpha = alpha_val)
      expect_equal(plt_lm$value, plt_fe$value, tolerance = 1e-4,
                   label = paste("contour", s, "alpha =", alpha_val))
    }
  }
})

test_that("sensemakr with alpha != 0.05: lm vs fixest", {
  for (alpha_val in c(0.01, 0.10)) {
    s_lm <- sensemakr(lm.out, treatment = "directlyharmed",
                       alpha = alpha_val, benchmark_covariates = "female", kd = 1:3)
    s_fe <- sensemakr(feols.out, treatment = "directlyharmed",
                       alpha = alpha_val, benchmark_covariates = "female", kd = 1:3)
    expect_equal(s_lm$sensitivity_stats$rv_qa, s_fe$sensitivity_stats$rv_qa,
                 tolerance = 1e-4, label = paste("alpha =", alpha_val))
    expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t,
                 tolerance = 1e-4, label = paste("bounds alpha =", alpha_val))
  }
})


# =========================================================================
# 5. reduce = FALSE
# =========================================================================

test_that("reduce = FALSE: lm vs fixest vs numeric for adjusted_ci", {
  for (treat in c("directlyharmed", "age")) {
    md <- model_helper(lm.out, covariates = treat)
    ci_lm  <- adjusted_ci(lm.out, treatment = treat,
                           r2dz.x = 0.05, r2yz.dx = 0.05, reduce = FALSE)
    ci_fe  <- adjusted_ci(feols.out, treatment = treat,
                           r2dz.x = 0.05, r2yz.dx = 0.05, reduce = FALSE)
    ci_num <- adjusted_ci(estimate = md$estimate, se = md$se, dof = md$dof,
                           r2dz.x = 0.05, r2yz.dx = 0.05, reduce = FALSE)
    expect_equal(unname(drop(ci_lm)), unname(drop(ci_num)), tolerance = 1e-10,
                 label = paste("ci reduce=F", treat))
    expect_equal(unname(drop(ci_fe)), unname(drop(ci_num)), tolerance = 1e-4,
                 label = paste("ci fe reduce=F", treat))
  }
})

test_that("reduce = FALSE increases absolute value of estimate", {
  for (treat in c("directlyharmed", "age")) {
    ae_reduce   <- adjusted_estimate(lm.out, treatment = treat,
                                      r2dz.x = 0.05, r2yz.dx = 0.05, reduce = TRUE)
    ae_increase <- adjusted_estimate(lm.out, treatment = treat,
                                      r2dz.x = 0.05, r2yz.dx = 0.05, reduce = FALSE)
    expect_true(abs(ae_increase) > abs(ae_reduce),
                label = paste("reduce=F bigger than reduce=T for", treat))
  }
})

test_that("sensemakr with reduce = FALSE: lm vs fixest", {
  s_lm <- sensemakr(lm.out, treatment = "directlyharmed", reduce = FALSE,
                     benchmark_covariates = "female", kd = 1:3)
  s_fe <- sensemakr(feols.out, treatment = "directlyharmed", reduce = FALSE,
                     benchmark_covariates = "female", kd = 1:3)
  expect_equal(s_lm$bounds$adjusted_estimate, s_fe$bounds$adjusted_estimate,
               tolerance = 1e-4)
  expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4)
})

test_that("contour plot with reduce = FALSE: lm vs fixest", {
  for (s in c("estimate", "t-value")) {
    plt_lm <- ovb_contour_plot(lm.out, treatment = "directlyharmed",
                                sensitivity.of = s, reduce = FALSE)
    plt_fe <- ovb_contour_plot(feols.out, treatment = "directlyharmed",
                                sensitivity.of = s, reduce = FALSE)
    expect_equal(plt_lm$value, plt_fe$value, tolerance = 1e-4,
                 label = paste("contour reduce=F", s))
  }
})

test_that("extreme plot with reduce = FALSE: lm vs fixest", {
  plt_lm <- ovb_extreme_plot(lm.out, treatment = "directlyharmed",
                              reduce = FALSE, r2yz.dx = c(1, 0.5))
  plt_fe <- ovb_extreme_plot(feols.out, treatment = "directlyharmed",
                              reduce = FALSE, r2yz.dx = c(1, 0.5))
  expect_true(!is.null(plt_lm))
  expect_true(!is.null(plt_fe))
})


# =========================================================================
# 6. invert = TRUE for robustness values
# =========================================================================

test_that("robustness_value invert = TRUE: lm vs fixest vs numeric", {
  md <- model_helper(lm.out, covariates = "directlyharmed")
  rv_lm  <- robustness_value(lm.out, covariates = "directlyharmed", invert = TRUE)
  rv_fe  <- robustness_value(feols.out, covariates = "directlyharmed", invert = TRUE)
  rv_num <- robustness_value(t_statistic = md$t_statistics, dof = md$dof, invert = TRUE)
  expect_equal(as.numeric(rv_lm), as.numeric(rv_num), tolerance = 1e-10)
  expect_equal(as.numeric(rv_fe), as.numeric(rv_num), tolerance = 1e-4)
})

test_that("extreme_robustness_value invert = TRUE: lm vs fixest vs numeric", {
  md <- model_helper(lm.out, covariates = "directlyharmed")
  xrv_lm  <- extreme_robustness_value(lm.out, covariates = "directlyharmed", invert = TRUE)
  xrv_fe  <- extreme_robustness_value(feols.out, covariates = "directlyharmed", invert = TRUE)
  xrv_num <- extreme_robustness_value(t_statistic = md$t_statistics, dof = md$dof, invert = TRUE)
  expect_equal(unname(xrv_lm), unname(xrv_num), tolerance = 1e-10)
  expect_equal(unname(xrv_fe), unname(xrv_num), tolerance = 1e-4)
})

test_that("sensemakr with invert = TRUE: lm vs fixest", {
  s_lm <- sensemakr(lm.out, treatment = "directlyharmed", invert = TRUE,
                     benchmark_covariates = "female", kd = 1:3)
  s_fe <- sensemakr(feols.out, treatment = "directlyharmed", invert = TRUE,
                     benchmark_covariates = "female", kd = 1:3)
  expect_equal(s_lm$sensitivity_stats, s_fe$sensitivity_stats, tolerance = 1e-4)
})


# =========================================================================
# 7. adjusted_critical_value with max = FALSE
# =========================================================================

test_that("adjusted_critical_value with max = FALSE", {
  acv_max  <- adjusted_critical_value(r2dz.x = 0.05, r2yz.dx = 0.05,
                                       dof = 783, alpha = 0.05, max = TRUE)
  acv_nomax <- adjusted_critical_value(r2dz.x = 0.05, r2yz.dx = 0.05,
                                        dof = 783, alpha = 0.05, max = FALSE)
  # Both should be numeric
  expect_true(is.numeric(acv_max))
  expect_true(is.numeric(acv_nomax))
  # max = TRUE should be >= max = FALSE (worst case is at least as extreme)
  expect_true(acv_max >= acv_nomax)

  # With zero confounding, adjusted_critical_value returns:
  #   t.crit * sqrt(dof/(dof-1)), where t.crit = sqrt(qf(alpha, 1, dof-1))
  # The sqrt(dof/(dof-1)) factor comes from sef*ddof in the formula (see bias_functions.R)
  acv_zero_max   <- adjusted_critical_value(r2dz.x = 0, r2yz.dx = 0,
                                             dof = 783, alpha = 0.05, max = TRUE)
  acv_zero_nomax <- adjusted_critical_value(r2dz.x = 0, r2yz.dx = 0,
                                             dof = 783, alpha = 0.05, max = FALSE)
  expected_crit <- sqrt(qf(0.95, df1 = 1, df2 = 782)) * sqrt(783 / 782)
  expect_equal(acv_zero_max, expected_crit, tolerance = 1e-10)
  expect_equal(acv_zero_nomax, expected_crit, tolerance = 1e-10)
})

test_that("adjusted_critical_value with vector inputs", {
  acv <- adjusted_critical_value(r2dz.x = c(0.01, 0.05, 0.1),
                                  r2yz.dx = c(0.01, 0.05, 0.1),
                                  dof = 783, alpha = 0.05)
  expect_length(acv, 3)
  # Should be monotonically increasing with confounding strength
  expect_true(all(diff(acv) > 0))
})


# =========================================================================
# 8. Edge values: zero confounding, large r2, small dof
# =========================================================================

test_that("zero confounding returns original values", {
  ae <- adjusted_estimate(estimate = 0.1, se = 0.02, dof = 783,
                           r2dz.x = 0, r2yz.dx = 0)
  expect_equal(ae, 0.1, tolerance = 1e-10)

  # Note: adjusted_se with zero confounding equals se * sqrt(dof/(dof-1))
  # because it uses the F-based formula, not exactly the original se
  ase <- adjusted_se(se = 0.02, dof = 783, r2dz.x = 0, r2yz.dx = 0)
  expect_equal(ase, 0.02 * sqrt(783 / 782), tolerance = 1e-10)

  # adjusted_t = adjusted_estimate / adjusted_se = estimate / (se * sqrt(dof/(dof-1)))
  at <- adjusted_t(estimate = 0.1, se = 0.02, dof = 783,
                    r2dz.x = 0, r2yz.dx = 0)
  expected_t <- 0.1 / (0.02 * sqrt(783 / 782))
  expect_equal(as.numeric(at), expected_t, tolerance = 1e-10)
})

test_that("small dof does not cause numerical issues", {
  # dof = 3 is very small
  ae <- adjusted_estimate(estimate = 1, se = 0.3, dof = 3,
                           r2dz.x = 0.05, r2yz.dx = 0.05)
  expect_true(is.finite(ae))

  at <- adjusted_t(estimate = 1, se = 0.3, dof = 3,
                    r2dz.x = 0.05, r2yz.dx = 0.05)
  expect_true(is.finite(as.numeric(at)))

  rv <- robustness_value(t_statistic = 3, dof = 3)
  expect_true(is.finite(rv) && rv >= 0)
})

test_that("large r2 values close to 1", {
  # r2dz.x = 0.99 should still work
  ae <- adjusted_estimate(estimate = 0.1, se = 0.02, dof = 783,
                           r2dz.x = 0.99, r2yz.dx = 0.01)
  expect_true(is.finite(ae))

  # Symmetric: r2yz.dx = 0.99
  ae2 <- adjusted_estimate(estimate = 0.1, se = 0.02, dof = 783,
                            r2dz.x = 0.01, r2yz.dx = 0.99)
  expect_true(is.finite(ae2))
})

test_that("vector r2 inputs to bias functions", {
  r2dz <- c(0.01, 0.05, 0.1)
  r2yz <- c(0.01, 0.05, 0.1)
  b <- bias(se = 0.02, dof = 783, r2dz.x = r2dz, r2yz.dx = r2yz)
  expect_length(b, 3)
  # Bias should be monotonically increasing with r2
  expect_true(all(diff(b) > 0))

  ae <- adjusted_estimate(estimate = 0.1, se = 0.02, dof = 783,
                           r2dz.x = r2dz, r2yz.dx = r2yz)
  expect_length(ae, 3)
})


# =========================================================================
# 9. Grouped benchmark covariates
# =========================================================================

test_that("grouped benchmarks: lm vs fixest", {
  bench_list <- list("age+farmer" = c("age", "farmer_dar"))
  bounds_lm <- ovb_bounds(lm.out, treatment = "directlyharmed",
                           benchmark_covariates = bench_list, kd = 1:2, ky = 1:2)
  bounds_fe <- ovb_bounds(feols.out, treatment = "directlyharmed",
                           benchmark_covariates = bench_list, kd = 1:2, ky = 1:2)
  expect_equal(bounds_lm$r2dz.x, bounds_fe$r2dz.x, tolerance = 1e-4)
  expect_equal(bounds_lm$r2yz.dx, bounds_fe$r2yz.dx, tolerance = 1e-4)
})

test_that("multiple grouped benchmarks", {
  bench_list <- list("age+farmer" = c("age", "farmer_dar"),
                     "voted+herder" = c("pastvoted", "herder_dar"))
  bounds <- ovb_bounds(lm.out, treatment = "directlyharmed",
                        benchmark_covariates = bench_list, kd = 1:2, ky = 1:2)
  # Should have 4 rows: 2 groups x 2 kd values
  expect_equal(nrow(bounds), 4)
  # Labels should include both group names
  expect_true(any(grepl("age\\+farmer", bounds$bound_label)))
  expect_true(any(grepl("voted\\+herder", bounds$bound_label)))
})


# =========================================================================
# 10. Combined edge cases: negative coef + q != 1 + reduce = FALSE
# =========================================================================

test_that("combined: negative coef + q = 0.5 + reduce = FALSE: lm vs fixest vs numeric", {
  md <- model_helper(lm.out, covariates = "age")
  s_lm  <- sensemakr(lm.out, treatment = "age", q = 0.5, reduce = FALSE,
                      benchmark_covariates = "female", kd = 1:3)
  s_fe  <- sensemakr(feols.out, treatment = "age", q = 0.5, reduce = FALSE,
                      benchmark_covariates = "female", kd = 1:3)
  s_num <- sensemakr(estimate = md$estimate, se = md$se, dof = md$dof,
                      treatment = "age", q = 0.5, reduce = FALSE,
                      benchmark_covariates = "female",
                      kd = 1:3,
                      r2dxj.x = get_r2dxj.x(lm.out, "age", "female"),
                      r2yxj.dx = partial_r2(lm.out, covariates = "female"))
  expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4,
               label = "combined neg+q+reduce lm vs fe")
  expect_equal(s_lm$bounds$adjusted_t, s_num$bounds$adjusted_t, tolerance = 1e-5,
               label = "combined neg+q+reduce lm vs num")
  expect_equal(s_lm$bounds$adjusted_estimate, s_num$bounds$adjusted_estimate, tolerance = 1e-5,
               label = "combined neg+q+reduce estimates lm vs num")
})

test_that("combined: positive coef + q = 2 + alpha = 0.01: lm vs fixest vs numeric", {
  md <- model_helper(lm.out, covariates = "directlyharmed")
  s_lm  <- sensemakr(lm.out, treatment = "directlyharmed", q = 2, alpha = 0.01,
                      benchmark_covariates = "female", kd = 1:3)
  s_fe  <- sensemakr(feols.out, treatment = "directlyharmed", q = 2, alpha = 0.01,
                      benchmark_covariates = "female", kd = 1:3)
  s_num <- sensemakr(estimate = md$estimate, se = md$se, dof = md$dof,
                      treatment = "directlyharmed", q = 2, alpha = 0.01,
                      benchmark_covariates = "female",
                      kd = 1:3,
                      r2dxj.x = get_r2dxj.x(lm.out, "directlyharmed", "female"),
                      r2yxj.dx = partial_r2(lm.out, covariates = "female"))
  expect_equal(s_lm$bounds$adjusted_t, s_fe$bounds$adjusted_t, tolerance = 1e-4)
  expect_equal(s_lm$bounds$adjusted_t, s_num$bounds$adjusted_t, tolerance = 1e-5)
  expect_equal(s_lm$sensitivity_stats$rv_qa, s_fe$sensitivity_stats$rv_qa, tolerance = 1e-4)
  expect_equal(s_lm$sensitivity_stats$rv_qa, s_num$sensitivity_stats$rv_qa, tolerance = 1e-5)
})
