context("Checking sensemakr")

test_that("Testing darfur 'female'", {
  # cleans workspace
  rm(list = ls())

  # library
  library(sensemakr)

  # loads data
  data("darfur")

  # fits model
  full.model  <- lm(peacefactor ~ directlyharmed + age + female + farmer_dar + herder_dar +
                      pastvoted + hhsize_darfur  + village, data = darfur)

  model  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                 pastvoted + hhsize_darfur  + village, data = darfur)


  # runs benchmarking etc
  sense <- sensemakr(model = model, treatment = "directlyharmed")
  methods(sensemakr)

  expect_that(sense, is_a("sensemakr"))


  # if want to test low level helper calculations
  # sourceable in sensemakr.R
  # they are not exported formally

  estimate <- coef(summary(model))[2,1]
  se <- coef(summary(model))[2,2]
  t <- coef(summary(model))[2,3]
  df <- df.residual(model)
  tf <- coef(summary(full.model))[4,3]
  r2y <- tf^2/(tf^2 + df.residual(full.model))
  treat.model <- lm(directlyharmed ~ age + female + farmer_dar + herder_dar +
                      pastvoted + hhsize_darfur  + village, data = darfur)
  tfd <- coef(summary(treat.model))[3,3]
  r2d <- tfd^2/(tfd^2 + df.residual(treat.model))

  ## low level helpers like get_bias() no longer exported
  ## if want to re-confirm calculations are correct, re-export helpers

  # bias <- get_bias(se, df, r2y, r2d)
  real_bias <- abs(unname(coef(full.model)[2] - coef(model)[2]))
  # expect_equal(bias, real_bias)

  # adj_se <- get_se(se, df, r2y, r2d)
  real_se <- coef(summary(full.model))[2,2]
  # expect_equal(real_se, adj_se)

  # adj_estimate <- estimate - get_bias(se, df, r2y, r2d)
  real_estimate <- unname(coef(full.model)[2])
  # expect_equal(adj_estimate, real_estimate)

  # adj_t  <- (estimate - get_bias(se, df, r2y, r2d))/get_se(se, df, r2y, r2d)
  # adj_t2 <- get_t(t, df, r2y, r2d)
  real_t <- coef(summary(full.model))[2,3]
  # expect_equal(adj_t, adj_t2)
  # expect_equal(adj_t2, real_t)

})

