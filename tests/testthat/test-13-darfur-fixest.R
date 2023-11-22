context("Testing Darfur Example with fixest model")
# runs regression model

data("darfur")

# fixest using village as fixed effects
model <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female|village, data = darfur)

# vanilla lm
model_lm <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                  pastvoted + hhsize_darfur + female + village, data = darfur)

# Model fixest using heteroskedastic se's
# -note adjusted estimates, partial R2, and the RV for point estimates should *always* use the "iid" SE.
# -this is because the SE is just a shortcut for computing the residual standard deviations.
# -Robust SEs should be used for inference (confidence intervals, RV_alpha),
# -but to do that properly we need to use the delta method.
# -this can be implemented later, for now sensemakr will simply ignore the "hetero" flag,
# -and provide a message to the user.
model_het <- fixest::feols(peacefactor ~ directlyharmed + age+  farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female | village, data = darfur,
                        vcov = "hetero")


test_that(desc = "Identical Results lm and feols with village fixed effects",
          {
            darfur_out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            darfur_out_lm <- sensemakr(model_lm, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot1 <- plot(darfur_out)
            plot1_lm <- plot(darfur_out_lm)
            expect_equal(plot1, plot1_lm)
            plot2 <- ovb_contour_plot(darfur_out)
            plot2_lm <- ovb_contour_plot(darfur_out_lm)
            expect_equal(plot2, plot2_lm)
            plot3 <- ovb_extreme_plot(darfur_out)
            plot3_lm <- ovb_extreme_plot(darfur_out_lm)
            expect_equal(plot3, plot3_lm)

            # info
            expect_equal(darfur_out$info$formula,
                         peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female | village,
                         darfur_out_lm$info$formula,
                          peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female + village)
            expect_equal(darfur_out$info$treatment, darfur_out_lm$info$treatment)
            expect_equal(darfur_out$info$q, darfur_out_lm$info$q)
            expect_equal(darfur_out$info$alpha, darfur_out_lm$info$alpha)
            expect_equal(darfur_out$info$reduce, darfur_out_lm$info$reduce)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, darfur_out_lm$sensitivity_stats$dof)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, darfur_out_lm$sensitivity_stats$r2yd.x, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), c(darfur_out_lm$sensitivity_stats$rv_q), tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), c(darfur_out_lm$sensitivity_stats$rv_qa), tolerance = 1e-5)

            # bounds
            expect_equivalent(darfur_out$bounds, darfur_out_lm$bounds)

            # out1 <- capture.output(darfur_out)
            # out1_lm <- capture.output(darfur_out_lm)
            # expect_equal(out1, out1_lm)
            # out2 <- capture.output(summary(darfur_out))
            # out2_lm <- capture.output(summary(darfur_out_lm))
            # expect_equal(out2, out2_lm)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            out3_lm <- capture.output(ovb_minimal_reporting(darfur_out_lm))
            expect_equal(out3, out3_lm)
            darfur_out2 <- sensemakr(model, treatment = "directlyharmed")
            darfur_out2_lm <- sensemakr(model_lm, treatment = "directlyharmed")
            plot1 <- plot(darfur_out2)
            plot1_lm <- plot(darfur_out2)
            expect_equal(plot1, plot1_lm)
            plot2 <- ovb_contour_plot(darfur_out2)
            plot2_lm <- ovb_contour_plot(darfur_out2_lm)
            expect_equal(plot2, plot2_lm)
            plot3 <- ovb_extreme_plot(darfur_out2)
            plot3_lm <- ovb_extreme_plot(darfur_out2_lm)
            expect_equal(plot3, plot3_lm)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))
            out3_lm <- capture.output(ovb_minimal_reporting(darfur_out2_lm))
            expect_equal(out3, out3_lm)
          })

test_that(desc = "Identifical results with using different vcov (we should ignore vcov for adjusted estimates)" ,
          {
            darfur_out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            darfur_out_het <- sensemakr(model_het, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot1 <- plot(darfur_out)
            plot1_lm <- plot(darfur_out_het)
            expect_equal(plot1, plot1_lm)
            plot2 <- ovb_contour_plot(darfur_out)
            plot2_lm <- ovb_contour_plot(darfur_out_het)
            expect_equal(plot2, plot2_lm)
            plot3 <- ovb_extreme_plot(darfur_out)
            plot3_lm <- ovb_extreme_plot(darfur_out_het)
            expect_equal(plot3, plot3_lm)

            # info
            expect_equal(darfur_out$info$formula,
                         peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female | village,
                         darfur_out_het$info$formula,
                         peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female + village)
            expect_equal(darfur_out$info$treatment, darfur_out_het$info$treatment)
            expect_equal(darfur_out$info$q, darfur_out_het$info$q)
            expect_equal(darfur_out$info$alpha, darfur_out_het$info$alpha)
            expect_equal(darfur_out$info$reduce, darfur_out_het$info$reduce)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, darfur_out_het$sensitivity_stats$dof)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, darfur_out_het$sensitivity_stats$r2yd.x, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), c(darfur_out_het$sensitivity_stats$rv_q), tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), c(darfur_out_het$sensitivity_stats$rv_qa), tolerance = 1e-5)

            # bounds
            expect_equivalent(darfur_out$bounds, darfur_out_het$bounds)

            out1 <- capture.output(darfur_out)
            out1_lm <- capture.output(darfur_out_het)
            expect_equal(out1, out1_lm)
            out2 <- capture.output(summary(darfur_out))
            out2_lm <- capture.output(summary(darfur_out_het))
            expect_equal(out2, out2_lm)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            out3_lm <- capture.output(ovb_minimal_reporting(darfur_out_het))
            expect_equal(out3, out3_lm)

            darfur_out2 <- sensemakr(model, treatment = "directlyharmed")
            darfur_out_het2 <- sensemakr(model_het, treatment = "directlyharmed")
            plot1 <- plot(darfur_out2)
            plot1_lm <- plot(darfur_out2)
            expect_equal(plot1, plot1_lm)
            plot2 <- ovb_contour_plot(darfur_out2)
            plot2_lm <- ovb_contour_plot(darfur_out_het2)
            expect_equal(plot2, plot2_lm)
            plot3 <- ovb_extreme_plot(darfur_out2)
            plot3_lm <- ovb_extreme_plot(darfur_out_het2)
            expect_equal(plot3, plot3_lm)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))
            out3_lm <- capture.output(ovb_minimal_reporting(darfur_out_het2))
            expect_equal(out3, out3_lm)

          })


test_that("testing darfur manual bounds",
          {
            sense.out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", r2dz.x = .1)
            bounds.check <- sense.out$bounds
            to_check     <- bounds.check$adjusted_se[1]
            true_check   <- adjusted_se(model, treatment = "directlyharmed", r2dz.x = .1, r2yz.dx = .1)
            expect_equal(to_check, unname(true_check))
          })


test_that(desc = "testing darfur sensemakr with formula",
          {
            darfur_out <- sensemakr(formula = peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                                      pastvoted + hhsize_darfur + female + village, data = darfur, method = "feols", treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)

            # info
            expect_equal(darfur_out$info$formula, peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female + village)
            expect_equal(darfur_out$info$treatment, "directlyharmed")
            expect_equal(darfur_out$info$q, 1)
            expect_equal(darfur_out$info$alpha, 0.05)
            expect_equal(darfur_out$info$reduce, TRUE)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, 783)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02187, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.13878, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.07626, tolerance = 1e-5)

            # bounds
            check_bounds <- structure(list(bound_label = c("1x female", "2x female", "3x female"),
                                           r2dz.x = c(0.00916428667504862, 0.0183285733500972, 0.0274928600251459),
                                           r2yz.dx = c(0.12464092303637, 0.249324064199975, 0.374050471038094),
                                           treatment = rep("directlyharmed", 3),
                                           adjusted_estimate = c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548),
                                           adjusted_se = c(0.0218733277437572, 0.0203500620779637, 0.0186700648170924),
                                           adjusted_t = c(3.43890386024675, 2.60024623913809, 1.62806202131271),
                                           adjusted_lower_CI = c(0.032282966, 0.012968035,-0.006253282),
                                           adjusted_upper_CI = c(0.11815758, 0.09286231, 0.06704533)),
                                      .Names = c("bound_label", "r2dz.x", "r2yz.dx", "treatment",
                                                 "adjusted_estimate", "adjusted_se", "adjusted_t",
                                                 "adjusted_lower_CI", "adjusted_upper_CI"),
                                      row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equivalent(darfur_out$bounds, check_bounds)
          })


test_that(desc = "testing darfur sensemakr manually",
          {

            model.treat <- fixest::feols(directlyharmed ~ age + farmer_dar + herder_dar +
                                pastvoted + hhsize_darfur + female | village, data = darfur)

            darfur_out <- sensemakr(estimate = 0.09731582,
                                    se = 0.02325654,
                                    dof = 783,
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    r2dxj.x = partial_r2(model.treat, covariates = "female"),
                                    r2yxj.dx = partial_r2(model, covariates = "female"),
                                    kd = 1:3)
            plot(darfur_out)
            plot(darfur_out, type = "extreme")
            plot(darfur_out, sensitivity.of = "t-value")
            add_bound_to_contour(0.3,0.3, "test")

            # info
            expect_equal(darfur_out$info$formula, "Data provided manually")
            expect_equal(darfur_out$info$treatment, "directlyharmed")
            expect_equal(darfur_out$info$q, 1)
            expect_equal(darfur_out$info$alpha, 0.05)
            expect_equal(darfur_out$info$reduce, TRUE)

            expect_equal(darfur_out$sensitivity_stats$dof, 783)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02187, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.13878, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.07626, tolerance = 1e-5)

            # bounds
            check_bounds <-
              structure(
                list(
                  bound_label = c("1x female", "2x female", "3x female"),
                  r2dz.x = c(0.00916428667504862, 0.0183285733500972, 0.0274928600251459),
                  r2yz.dx = c(0.12464092303637, 0.249324064199975, 0.374050471038094),
                  adjusted_estimate = c(0.0752202698486415, 0.0529151689180575,
                                        0.0303960178770157),
                  adjusted_se = c(0.0218733298036818, 0.0203500639944344,
                                  0.0186700665753491),
                  adjusted_t = c(3.43890347394571, 2.60024582392121,
                                 1.62806156873318),
                  adjusted_lower_CI = c(0.0322829603180086,
                                        0.0129680276030601, -0.00625329133645187),
                  adjusted_upper_CI = c(0.118157579379274,
                                        0.092862310233055, 0.0670453270904833)
                ),
                row.names = c(NA, -3L),
                class = "data.frame"
              )

            expect_equivalent(as.data.frame(darfur_out$bounds), as.data.frame(check_bounds), tolerance = 1e-5)
          })


test_that(desc = "testing darfur sensitivity stats",
          {

            # checks RV
            ## RV q = 1
            rv <- robustness_value(model, covariates = "directlyharmed", alpha = 1)
            expect_equivalent(c(rv), 0.138776, tolerance = 1e-5)
            expect_equivalent(attributes(rv)$q, 1)
            expect_equivalent(attributes(rv)$names, "directlyharmed")

            ## RV q = 1, alpha = 0.05
            rv <- robustness_value(model, covariates = "directlyharmed", q = 1, alpha = 0.05)
            expect_equivalent(c(rv),  0.07625797, tolerance = 1e-5)
            expect_equivalent(attributes(rv)$q, 1)
            expect_equivalent(attributes(rv)$alpha, 0.05)
            expect_equivalent(attributes(rv)$names, "directlyharmed")

            # checks partial R2
            r2 <- partial_r2(model, covariates = "directlyharmed")
            expect_equivalent(r2, 0.02187309, tolerance = 1e-5)

            # checks partial f2
            f2 <- partial_f2(model, covariates = "directlyharmed")
            expect_equivalent(f2,  0.02236222, tolerance = 1e-5)

            # sensitivity stats
            sens_stats <- sensitivity_stats(model, treatment = "directlyharmed")
            expect_equivalent(sens_stats$treatment, "directlyharmed")
            expect_equivalent(sens_stats$estimate, 0.09731582, tolerance = 1e5)
            expect_equivalent(sens_stats$se, 0.02325654, tolerance = 1e5)
            expect_equivalent(sens_stats$t_statistic, 4.18445, tolerance = 1e5)
            expect_equivalent(sens_stats$r2yd.x, 0.02187309, tolerance = 1e5)
            expect_equivalent(sens_stats$rv_q , 0.1387764, tolerance = 1e5)
            expect_equivalent(sens_stats$rv_qa , 0.07625797, tolerance = 1e5)
            expect_equivalent(sens_stats$f2yd.x , 0.02236222, tolerance = 1e5)
            expect_equivalent(sens_stats$dof , 783, tolerance = 1e5)

            expect_equivalent(group_partial_r2(model, covariates = "directlyharmed"), partial_r2(model, covariates = "directlyharmed"), tolerance = 1e5)
            expect_error(group_partial_r2(model))
            expect_equal(group_partial_r2(model, covariates = c("directlyharmed", "female")), 0.1350435, tolerance = 1e-5)

          })

test_that(desc = "testing darfur adjusted estimates",
          {

            should_be_zero <- adjusted_estimate(model, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model, covariates = "directlyharmed"))
            expect_equivalent(should_be_zero, 0)

            rv <- robustness_value(model, covariates = "directlyharmed", alpha = 1)
            should_be_zero <- adjusted_estimate(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_zero, 0)


            rv <- robustness_value(model, covariates = "directlyharmed", alpha = 0.05)
            thr <- qt(0.975, df = 783 - 1)
            should_be_1.96 <- adjusted_t(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(c(should_be_1.96), thr)

            should_be_estimate <- bias(model, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model, covariates = "directlyharmed"))
            expect_equivalent(should_be_estimate, coef(model)["directlyharmed"])

            rv <- robustness_value(model, covariates = "directlyharmed", alpha = 1)
            should_be_estimate <- bias(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_estimate, coef(model)["directlyharmed"])

            rv <- robustness_value(model, covariates = "directlyharmed", q = 0.5, alpha = 1)
            should_be_half_estimate <- bias(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_half_estimate, coef(model)["directlyharmed"]/2)

          })

test_that(desc = "testing darfur plots",
          {

            # testing countour output with internal functions
            ## point estimate
            contour_out <- ovb_contour_plot(model, treatment = "directlyharmed",
                                            benchmark_covariates = "female",kd = 1:3)
            add_bound_to_contour(model, treatment = "directlyharmed",
                                 benchmark_covariates = "age", kd = 10)
            add_bound_to_contour(model, treatment = "directlyharmed",
                                 benchmark_covariates = "age", kd = 200, ky = 20)
            adj_est <- adjusted_estimate(model, treatment = "directlyharmed",
                                         r2dz.x = contour_out$r2dz.x[10],
                                         r2yz.dx = contour_out$r2yz.dx[1])
            expect_equivalent(adj_est, contour_out$value[10])
            bounds <- ovb_bounds(model,
                                 treatment = "directlyharmed",
                                 benchmark_covariates = "female",
                                 kd = 1:3)
            expect_equivalent(contour_out$bounds, bounds[c(2,3,1)])

            ## t-value
            contour_out <- ovb_contour_plot(model, treatment = "directlyharmed",
                                            benchmark_covariates = "female",kd = 1:3,
                                            sensitivity.of = "t-value")
            add_bound_to_contour(model, treatment = "directlyharmed", benchmark_covariates = "age",
                                 kd = 200, ky = 10, sensitivity.of = "t-value")

            adj_t <- adjusted_t(model, treatment = "directlyharmed",
                                r2dz.x = contour_out$r2dz.x[10],
                                r2yz.dx = contour_out$r2yz.dx[1])
            expect_equivalent(adj_t, contour_out$value[10])
            bounds <- ovb_bounds(model,
                                 treatment = "directlyharmed",
                                 benchmark_covariates = "female",
                                 kd = 1:3)
            expect_equivalent(contour_out$bounds, bounds[c(2,3,1)])


            # tests bounds numerically
            # tests bounds numerically
            expect_equivalent(bounds$adjusted_estimate, c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548))
            expect_equivalent(bounds$adjusted_t, c(3.43890386024675, 2.60024623913809, 1.62806202131271))

            # test extreme scenario plot
            extreme_out <- ovb_extreme_plot(model, treatment = "directlyharmed", kd = 1:3)
            adj_est <- adjusted_estimate(model, treatment = "directlyharmed",
                                         r2yz.dx = 1,
                                         r2dz.x = extreme_out$scenario_r2yz.dx_1$r2dz.x[5])
            expect_equivalent(adj_est, extreme_out$scenario_r2yz.dx_1$adjusted_estimate[5])
          })


test_that("testing darfur different q",
          {
            darfur_out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", q = 2, kd = 1:3)
            rvq <- darfur_out$sensitivity_stats$rv_q
            rvqa <- darfur_out$sensitivity_stats$rv_qa
            expect_equivalent(rvq, robustness_value(model, covariates = "directlyharmed", q = 2, alpha = 1))
            expect_equivalent(rvqa, robustness_value(model, covariates = "directlyharmed", q = 2,alpha = 0.05))
          })

model5 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

test_that("Darfur group benchmarks",
          {
            village <- grep(pattern = "village", names(coef(model5)), value = T)

            village_lm <- grep(pattern = "village", names(coef(model_lm)), value = T)
            sensitivity.lm <- sensemakr(model_lm, treatment = "directlyharmed",
                                     benchmark_covariates = list(village = village_lm),
                                     kd = 0.3)
            sensitivity <- sensemakr(model5, treatment = "directlyharmed",
                                     benchmark_covariates = list(village = village),
                                     kd = 0.3)
            expect_equal(sensitivity.lm$bounds, sensitivity$bounds)
            r2y <- group_partial_r2(model5, covariates = village)
            treat.model <- update(model5, directlyharmed ~ .-directlyharmed)
            r2d <- group_partial_r2(treat.model, covariates = village)
            bounds.check <- ovb_partial_r2_bound(r2dxj.x = r2d, r2yxj.dx = r2y, kd = 0.3)
            bounds <- sensitivity$bounds
            expect_equal(bounds$r2dz.x, bounds.check$r2dz.x)
            expect_equal(bounds$r2yz.dx, bounds.check$r2yz.dx)

          })
