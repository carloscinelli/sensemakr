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
