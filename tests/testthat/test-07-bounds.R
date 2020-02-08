context("Testing Bounds")

test_that("Significance level of bounds",
          {
            data("darfur")
            # runs regression model
            model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female")

            out2 <- structure(list(bound_label = "1x female", r2dz.x = 0.00916428667504862,
                                   r2yz.dx = 0.12464092303637, adjusted_estimate = 0.0752202712144491,
                                   adjusted_se = 0.0218733277437572, adjusted_t = 3.43890386024675,
                                   adjusted_lower_CI = 0.0322829657274445, adjusted_upper_CI = 0.118157576701454),
                              row.names = "female", class = c("ovb_bounds","data.frame"))
            expect_equal(out, out2)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female", alpha = 0.2)
            out2 <- structure(list(bound_label = "1x female", r2dz.x = 0.00916428667504862,
                                   r2yz.dx = 0.12464092303637, adjusted_estimate = 0.0752202712144491,
                                   adjusted_se = 0.0218733277437572, adjusted_t = 3.43890386024675,
                                   adjusted_lower_CI = 0.0471648038348768, adjusted_upper_CI = 0.103275738594021),
                              row.names = "female", class = c("ovb_bounds", "data.frame"))
            expect_equal(out, out2)

            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female", alpha = 1)
            expect_equal(out$adjusted_estimate, out$adjusted_lower_CI)
            expect_equal(out$adjusted_estimate, out$adjusted_upper_CI)
          })
