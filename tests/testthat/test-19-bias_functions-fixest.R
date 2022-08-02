context("Bias functions")

test_that("Adjusted Partial R2",{

  resid_maker <- function(var) c(resid(lm(rnorm(n)~ var)))
  rcoef <- function() runif(1, min = -1, max = 1)

  # simulate data
  n <- 1e2
  z <- rnorm(n)
  d <- rcoef()*z + resid_maker(z)
  y <- rcoef()*d + rcoef()*z + resid_maker(cbind(d,z))

  # restricted model
  data = data.frame(y, d, z)
  model.r <- fixest::feols(y  ~ d, data = data)

  # full model
  model <- fixest::feols(y ~ d + z, data = data)

  # treat reg
  treat.reg <- fixest::feols(d ~ z, data = data)

  # true partials
  r2yd   <- partial_r2(model.r, covariates = "d")
  r2yd.z <- partial_r2(model, covariates = "d")

  # true confounder strength
  r2yz.d <- partial_r2(model, covariates = "z")
  r2d.z  <- partial_r2(treat.reg, covariates = "z")
  reduce <- rel_bias(coef(model.r)["d"], coef(model)["d"]) > 0

  adj.r2 <- adjusted_partial_r2(model.r, treatment = "d", r2dz.x = r2d.z, r2yz.dx = r2yz.d, reduce = reduce)
  expect_equivalent(adj.r2, unname(r2yd.z))
})
