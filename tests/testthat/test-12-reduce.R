context("Negative values and reduce = FALSE")

test_that("Symmetry", {
  data("darfur")
  library(sensemakr)
  darfur2 <- darfur
  darfur2$directlyharmed <- darfur2$directlyharmed*(-1)
  model <-  lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                 pastvoted + hhsize_darfur + female + village, data = darfur)
  model2 <-  lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                 pastvoted + hhsize_darfur + female + village, data = darfur2)
  sens1 <- sensemakr(model = model, treatment = "directlyharmed", q = 1)
  sens2 <- sensemakr(model = model2, treatment = "directlyharmed", q = 1)
  expect_equal(abs(sens1$sensitivity_stats[-1]), abs(sens2$sensitivity_stats[-1]))

  sens3 <- sensemakr(model = model, treatment = "directlyharmed", q = 1, reduce = F)
  sens4 <- sensemakr(model = model2, treatment = "directlyharmed", q = 1, reduce = F)

  expect_equal(abs(sens3$sensitivity_stats[-1]), abs(sens1$sensitivity_stats[-1]))
  expect_equal(abs(sens4$sensitivity_stats[-1]), abs(sens2$sensitivity_stats[-1]))

})

test_that("reduce = FALSE", {
  data("darfur")
  library(sensemakr)
  darfur2 <- darfur
  darfur2$directlyharmed <- darfur2$directlyharmed*(-1)
  model <-  lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                 pastvoted + hhsize_darfur + female + village, data = darfur)
  sens1 <- sensemakr(model = model, treatment = "directlyharmed", q = 1)
  sens2 <- sensemakr(model = model, treatment = "directlyharmed", q = 1, reduce = FALSE)
  expect_true(sens1$sensitivity_stats$t_statistic > 0)
  expect_true(sens2$sensitivity_stats$t_statistic < 0 )
  expect_equal(abs(sens1$sensitivity_stats[-1]), abs(sens2$sensitivity_stats[-1]))


  sens1 <- sensemakr(model = model, treatment = "directlyharmed", q = 2, benchmark_covariates = "female", kd = 1:3)
  sens2 <- sensemakr(model = model, treatment = "directlyharmed", q = 2, benchmark_covariates = "female", kd = 1:3, reduce = FALSE)
  h01 <- attr(sens1$bounds$adjusted_t, "h0")
  h02 <- attr(sens2$bounds$adjusted_t, "h0")
  expect_equal(h01, sens1$sensitivity_stats$estimate*(1-2))
  expect_equal(h02, sens1$sensitivity_stats$estimate*(1+2))
  expect_true(sens2$sensitivity_stats$t_statistic < 0 )
  expect_equal(abs(sens1$sensitivity_stats[-1]), abs(sens2$sensitivity_stats[-1]))
  ts <- with(sens1$bounds, (adjusted_estimate-h01)/adjusted_se)
  expect_equivalent(ts, sens1$bounds$adjusted_t)
})
