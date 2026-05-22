context("Weighted (WLS) benchmark bounds")

# A weighted regression and its frequency-expanded (row-replicated) unweighted
# counterpart are numerically the same regression, so ovb_bounds() must return
# the same benchmark bounds for both. Before the weights fix it did not: the
# internal auxiliary treatment regression in ovb_partial_r2_bound() dropped the
# model's weights, so the benchmark's partial R2 with the treatment (r2dz.x) --
# and hence the bounds and adjusted estimates -- were computed unweighted.

test_that("ovb_bounds respects lm() weights", {
  data("darfur")
  set.seed(1)
  darfur$wts <- sample(1:4, nrow(darfur), replace = TRUE)   # integer weights
  fml <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
    pastvoted + hhsize_darfur + female + village

  model_w <- lm(fml, data = darfur, weights = wts)
  model_e <- lm(fml, data = darfur[rep(seq_len(nrow(darfur)), darfur$wts), ])

  # single-covariate benchmark
  bw <- ovb_bounds(model_w, "directlyharmed", benchmark_covariates = "female")
  be <- ovb_bounds(model_e, "directlyharmed", benchmark_covariates = "female")
  expect_equal(bw$r2dz.x,            be$r2dz.x,            tolerance = 1e-6)
  expect_equal(bw$r2yz.dx,           be$r2yz.dx,           tolerance = 1e-6)
  expect_equal(bw$adjusted_estimate, be$adjusted_estimate, tolerance = 1e-6)

  # grouped benchmark
  gw <- ovb_bounds(model_w, "directlyharmed",
                   benchmark_covariates = list(grp = c("female", "age")))
  ge <- ovb_bounds(model_e, "directlyharmed",
                   benchmark_covariates = list(grp = c("female", "age")))
  expect_equal(gw$r2dz.x,            ge$r2dz.x,            tolerance = 1e-6)
  expect_equal(gw$adjusted_estimate, ge$adjusted_estimate, tolerance = 1e-6)
})

test_that("ovb_bounds respects fixest weights", {
  skip_if_not_installed("fixest")
  data("darfur")
  set.seed(1)
  darfur$wts <- sample(1:4, nrow(darfur), replace = TRUE)
  fml <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
    pastvoted + hhsize_darfur + female | village

  model_w <- fixest::feols(fml, data = darfur, weights = ~wts)
  model_e <- fixest::feols(fml, data = darfur[rep(seq_len(nrow(darfur)), darfur$wts), ])

  bw <- ovb_bounds(model_w, "directlyharmed", benchmark_covariates = "female")
  be <- ovb_bounds(model_e, "directlyharmed", benchmark_covariates = "female")
  expect_equal(bw$r2dz.x,            be$r2dz.x,            tolerance = 1e-5)
  expect_equal(bw$adjusted_estimate, be$adjusted_estimate, tolerance = 1e-5)
})
