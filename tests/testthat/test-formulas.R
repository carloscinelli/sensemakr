context("Checking formulas")

test_that("Testing R2 formulas", {
  rm(list = ls())
  library(sensemakr)
  library(testthat)
  model <- lm(mtcars)
  model.sum <- summary(model)
  coefs <- coef(model.sum)
  df <- df.residual(model)

  t <- coefs[-1,3]
  r2y   <-  t_to_r2(t, df)
  formulas <- lapply(2:nrow(coefs), function(i)
    as.formula(paste0(names(mtcars)[1], "~", paste0(names(mtcars)[c(-1, -i)], collapse = "+"))))
  models <- lapply(formulas, function(form) lm(form, mtcars))
  r2with <- summary(model)$r.
  r2wo <- sapply(models, function(x) summary(x)$r.)
  r2yn <- (r2with - r2wo)/(1-r2wo)
  expect_equal(r2yn, as.vector(r2y))

  treat.model <- lm(cyl ~ disp + hp + drat + wt + qsec + vs + am + gear + carb, mtcars)
  coef.treat <- coef(summary(treat.model))
  td <- coef.treat[-1, 3]
  r2d <- unname(t_to_r2(td, df.residual(treat.model)))

  formulasd <- lapply(3:nrow(coefs), function(i)
    as.formula(paste0(names(mtcars)[2], "~", paste0(names(mtcars)[c(-1, -2,  -i)], collapse = "+"))))
  modelsd <- lapply(formulasd, function(form) lm(form, mtcars))
  r2dwith <- summary(treat.model)$r.
  r2dwo <- sapply(modelsd, function(x) summary(x)$r.)
  r2dn <- (r2dwith - r2dwo)/(1-r2dwo)
  expect_equal(r2dn, r2d)

  estimate <- coefs[2, 1]
  se  <- sapply(models[-1], function(x) coef(summary(x))[2,2])
  dfs <- sapply(models[-1], function(x) df.residual(x))
  biases <- unname(get_bias(se, dfs, r2d = r2d, r2y = r2y[-1]))
  old_estimates <- sapply(models[-1], function(x) coef(x)[2])
  real_biases <- unname(abs(estimate - old_estimates))
  expect_equal(biases, real_biases)

  ses2 <- unname(get_se(se, dfs, r2d = r2d, r2y = r2y[-1]))
  expect_equal(ses2, rep(coefs[2, 2], length(ses2)))

  ts  <- sapply(models[-1], function(x) coef(summary(x))[2,3])
  new_t <- abs(unname(get_t(abs(ts), dfs, r2d = r2d, r2y = r2y[-1])))
  expect_equal(new_t, rep(abs(coefs[2, 3]), length(ses2)))
})
