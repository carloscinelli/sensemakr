context("Testing Bounds")

test_that("Significance level of bounds",
          {
            data("darfur")
            # runs regression model
            model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female")
            plot.env$treatment <- NULL
            expect_error(add_bound_to_contour(out))
            expect_error(add_bound_to_contour(model = model, benchmark_covariates = "female"))

            out.g <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = list(female = "female"))

            out2 <- structure(list(bound_label = "1x female", r2dz.x = 0.00916428667504862,
                                   r2yz.dx = 0.12464092303637, treatment = "directlyharmed",
                                   adjusted_estimate = 0.0752202712144491,
                                   adjusted_se = 0.0218733277437572, adjusted_t = 3.43890386024675,
                                   adjusted_lower_CI = 0.0322829657274445, adjusted_upper_CI = 0.118157576701454),
                              row.names = c(NA, -1L), class = c("ovb_bounds","data.frame"))
            expect_equivalent(out, out2)
            expect_equivalent(out2, out.g)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female", alpha = 0.2)
            out2 <- structure(list(bound_label = "1x female", r2dz.x = 0.00916428667504862,
                                   r2yz.dx = 0.12464092303637, treatment = "directlyharmed",
                                   adjusted_estimate = 0.0752202712144491,
                                   adjusted_se = 0.0218733277437572, adjusted_t = 3.43890386024675,
                                   adjusted_lower_CI = 0.0471648038348768, adjusted_upper_CI = 0.103275738594021),
                              row.names = c(NA, -1L),
                               class = c("ovb_bounds", "data.frame"))
            expect_equivalent(out, out2)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female", alpha = 1)
            expect_equal(out$adjusted_estimate, out$adjusted_lower_CI)
            expect_equal(out$adjusted_estimate, out$adjusted_upper_CI)
          })

test_that("Bounds error",
          {

            expect_error(ovb_partial_r2_bound.numeric(r2dxj.x = "a", r2yxj.dx = .2))
            expect_error(ovb_partial_r2_bound.numeric(r2dxj.x = .1, r2yxj.dx = 2))
            expect_warning(ovb_partial_r2_bound.numeric(r2dxj.x = .1, r2yxj.dx = 1))
            expect_error(ovb_partial_r2_bound.numeric(r2dxj.x = 1, r2yxj.dx = .1))
            data("darfur")
            # runs regression model
            model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)
            expect_error(ovb_partial_r2_bound.lm(model = model, treatment = 2, benchmark_covariates = "female"))
            expect_error(ovb_partial_r2_bound.lm(model = model, treatment = "directlyharmed", benchmark_covariates = 2))
            expect_error(ovb_partial_r2_bound.lm(model = model, treatment = c("a","b"),
                                                 benchmark_covariates = 2))

            expect_error(ovb_partial_r2_bound.lm(model = model, treatment = "directlyharmed",
                                                 benchmark_covariates = list(1)))

            expect_error(ovb_partial_r2_bound.lm(model = model, treatment = "directlyharmed",
                                                 benchmark_covariates = list(a  = 1, c= "a")))

          })


test_that("Bounds warning",
          {
             model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

              b1 <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female")
              ovb_contour_plot(model, treatment = "female")
              expect_warning(add_bound_to_contour(b1))
              expect_warning(add_bound_to_contour(model = model,
                                                  treatment = "directlyharmed",
                                                  benchmark_covariates = "female"))


              b1 <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female")
              ovb_contour_plot(model, treatment = "female", sensitivity.of = "t-value")
              expect_warning(add_bound_to_contour(b1))

              b2 <- ovb_partial_r2_bound(.1, .1)
              add_bound_to_contour(b2)


          })



test_that("Group benchmarks",
          {
             model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

             check1 <- ovb_contour_plot(model = model,
                               treatment = "directlyharmed",
                               benchmark_covariates = list(`fem+past` = c("female", "pastvoted")))

              out <- sensemakr(model = model,
                               treatment = "directlyharmed",
                               benchmark_covariates = list(`fem+past` = c("female", "pastvoted")))
              check2 <- plot(out)
              expect_equal(check1, check2)

          })


test_that("Group benchmarks - simulations",
          {

            # exact
            rm(list = ls())
            set.seed(10)
            n <- 1e3
            z1 <- resid_maker(n, rep(1, n))
            z2 <- resid_maker(n, z1)
            x1 <- resid_maker(n, cbind(z1, z2))
            x2 <- resid_maker(n, cbind(z1, z2, x1))
            d  <- 2*x1 + 1*x2 + 2*z1 + 1*z2 + resid_maker(n, cbind(z1, z2, x1, x2))*5
            y  <- 2*x1 + 1*x2 + 2*z1 + 1*z2 + resid_maker(n, cbind(z1, z2, x1, x2, d))*5
            model <- lm(y ~ d + x1 + x2)
            r2yx <- group_partial_r2(lm(y ~ d + x1 + x2), covariates = c("x1", "x2"))
            r2yz <- group_partial_r2(lm(y ~ d + z1 + z2), covariates = c("z1", "z2"))
            ky <- r2yz/r2yx

            r2dz <- group_partial_r2(lm(d ~ z1 + z2), covariates = c("z1", "z2"))
            r2dx <- group_partial_r2(lm(d ~ x1 + x2), covariates = c("x1", "x2"))
            kd <- r2dz/r2dx
            out   <- sensemakr(model = model, treatment = "d", benchmark_covariates = list(X = c("x1", "x2")), kd =kd, ky= ky)
            expect_equivalent(out$bounds$adjusted_estimate, 0)

            # conservative
            rm(list = ls())
            rcoef <- function() runif(1, -2, 2)
            n <- 1e3
            z1 <- resid_maker(n, rep(1, n))
            z2 <- resid_maker(n, z1)
            x1 <- resid_maker(n, cbind(z1, z2))
            x2 <- resid_maker(n, cbind(z1, z2, x1))
            d  <- rcoef()*x1 + rcoef()*x2 + rcoef()*z1 + rcoef()*z2 + resid_maker(n, cbind(z1, z2, x1, x2))*5
            y  <- rcoef()*x1 + rcoef()*x2 + rcoef()*z1 + rcoef()*z2 + resid_maker(n, cbind(z1, z2, x1, x2, d))*5
            model <- lm(y ~ d + x1 + x2)
            r2yx <- group_partial_r2(lm(y ~ d + x1 + x2), covariates = c("x1", "x2"))
            r2yz <- group_partial_r2(lm(y ~ d + z1 + z2), covariates = c("z1", "z2"))
            ky <- r2yz/r2yx

            r2dz <- group_partial_r2(lm(d ~ z1 + z2), covariates = c("z1", "z2"))
            r2dx <- group_partial_r2(lm(d ~ x1 + x2), covariates = c("x1", "x2"))
            kd <- r2dz/r2dx
            out   <- sensemakr(model = model, treatment = "d", benchmark_covariates = list(X = c("x1", "x2")), kd =kd, ky= ky)
            full.model <- lm(y ~ d + x1 + x2 + z1 + z2)
            true_r2yz.dx <- group_partial_r2(full.model, covariates = c("z1", "z2"))
            full.model.d <-lm(d ~ x1 + x2 + z1 + z2)
            true_r2dz.x <- group_partial_r2(full.model.d, covariates = c("z1", "z2"))
            expect_equal(out$bounds$r2dz.x, true_r2dz.x)
            expect_true(out$bounds$r2yz.dx > true_r2yz.dx)

          })




test_that("Factor treatment and factor benchmarks",{

  data(mtcars)
  mtcars$cyl <- as.factor(mtcars$cyl)
  mtcars$gear <- as.factor(mtcars$gear)

  model <- lm(mpg ~ cyl + gear, data = mtcars)
  sens <- sensemakr(model, treatment = "cyl6", benchmark_covariates = list(gear = c("gear4", "gear5")))
  sens
  coef.summ <- coef(summary(model))
  t.value <- coef.summ["cyl6", "t value"]
  rv <- robustness_value(t.value, model$df.residual, alpha = 1)
  rv.check <- sens$sensitivity_stats$rv_q
  expect_equal(rv, rv.check)

  # benchmarks
  r2y <- group_partial_r2(model, covariates = c("gear4", "gear5"))
  mtcars <- cbind(mtcars, model.matrix(~cyl + gear + 0, data = mtcars))
  treat.model <- lm(cyl6 ~ cyl8 + gear4 + gear5, data= mtcars)
  r2d <- group_partial_r2(treat.model, covariates = c("gear4", "gear5"))
  bounds <- sensemakr:::ovb_partial_r2_bound.numeric(r2dxj.x = r2d, r2yxj.dx = r2y)
  bounds.check <- sens$bounds
  expect_equal(bounds$r2dz.x, bounds.check$r2dz.x)
  expect_equal(bounds$r2yz.dx, bounds.check$r2yz.dx)
  adj.est<- adjusted_estimate(model, treatment = "cyl6", r2dz.x = bounds$r2dz.x, r2yz.dx = bounds$r2yz.dx)
  expect_equivalent(adj.est, bounds.check$adjusted_estimate)

})






