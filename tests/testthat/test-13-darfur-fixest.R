context("Testing Darfur Example with fixest model")
# runs regression model

data("darfur")

model <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female, data = darfur)

model_lm <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                  pastvoted + hhsize_darfur + female, data = darfur)

model2 <- fixest::feols(peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female | village + age, data = darfur)

model3 <- fixest::feols(peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female | village + age, data = darfur, vcov = "hetero")

model4 <-  fixest::feols(peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female | village + age,
                         data = within(darfur,  directlyharmed <- directlyharmed*(-1)))

model5 <- fixest::feols(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                         pastvoted + hhsize_darfur + female + village, data = darfur)


test_that(desc = "same results lm and feols",
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
            expect_equal(darfur_out$info$formula, peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female,
                         darfur_out_lm$info$formula, peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female)
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

            out1 <- capture.output(darfur_out)
            out1_lm <- capture.output(darfur_out_lm)
            expect_equal(out1, out1_lm)
            out2 <- capture.output(summary(darfur_out))
            out2_lm <- capture.output(summary(darfur_out_lm))
            expect_equal(out2, out2_lm)
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

test_that(desc = "testing darfur sensemakr with fixed effexts",
          {
            darfur_out <- sensemakr(model2, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot(darfur_out)
            ovb_contour_plot(darfur_out)
            ovb_extreme_plot(darfur_out)

            # info
            expect_equal(darfur_out$info$formula, peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female | village + age)
            expect_equal(darfur_out$info$treatment, "directlyharmed")
            expect_equal(darfur_out$info$q, 1)
            expect_equal(darfur_out$info$alpha, 0.05)
            expect_equal(darfur_out$info$reduce, TRUE)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, 485)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02760859, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.1549014, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.0761147, tolerance = 1e-5)

            # bounds
            check_bounds <- structure(list(bound_label = c("1x female", "2x female", "3x female"),
                                           r2dz.x = c(0.009586695, 0.019173391, 0.028760086),
                                           r2yz.dx = c(0.1520541, 0.3041646, 0.4563329),
                                           treatment = rep("directlyharmed", 3),
                                           adjusted_estimate = c(0.07057237, 0.04956097, 0.02833820),
                                           adjusted_se = c(0.02280808, 0.02076204, 0.01844235),
                                           adjusted_t = c(3.094183, 2.387096, 1.536583),
                                           adjusted_lower_CI = c(0.025757524, 0.008766321, -0.007898566),
                                           adjusted_upper_CI = c(0.11538722, 0.09035563, 0.06457496)
            ),
            .Names = c("bound_label", "r2dz.x", "r2yz.dx", "treatment",
                       "adjusted_estimate", "adjusted_se", "adjusted_t",
                       "adjusted_lower_CI", "adjusted_upper_CI"),
            row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equivalent(darfur_out$bounds, check_bounds, tolerance = 1e-5)

            out1 <- capture.output(darfur_out)
            out1 <- capture.output(summary(darfur_out))
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            darfur_out2 <- sensemakr(model2, treatment = "directlyharmed")
            plot(darfur_out2)
            ovb_contour_plot(darfur_out2)
            ovb_extreme_plot(darfur_out2)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))

          })

test_that(desc = "testing darfur sensemakr with fixed effexts and se adjustement",
          {
            darfur_out <- sensemakr(model3, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot(darfur_out)
            ovb_contour_plot(darfur_out)
            ovb_extreme_plot(darfur_out)

            # info
            expect_equal(darfur_out$info$formula, peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female | village + age)
            expect_equal(darfur_out$info$treatment, "directlyharmed")
            expect_equal(darfur_out$info$q, 1)
            expect_equal(darfur_out$info$alpha, 0.05)
            expect_equal(darfur_out$info$reduce, TRUE)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, 716)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.01196593, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.1041604, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.03596192, tolerance = 1e-5)

            # bounds
            check_bounds <- structure(list(bound_label = c("1x female", "2x female", "3x female"),
                                           r2dz.x = c(0.004313658, 0.008627316, 0.012940974),
                                           r2yz.dx = c(0.06566111, 0.13132712, 0.19699810),
                                           treatment = rep("directlyharmed", 3),
                                           adjusted_estimate = c(0.0773726, 0.06330679, 0.04917896),
                                           adjusted_se = c(0.03008068, 0.02906742, 0.02800809),
                                           adjusted_t = c(2.572169, 2.17793, 1.755884),
                                           adjusted_lower_CI = c(0.01831573, 0.006239226, -0.005808839),
                                           adjusted_upper_CI = c(0.1364295, 0.1203744, 0.1041668)
            ),
            .Names = c("bound_label", "r2dz.x", "r2yz.dx", "treatment",
                       "adjusted_estimate", "adjusted_se", "adjusted_t",
                       "adjusted_lower_CI", "adjusted_upper_CI"),
            row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equivalent(darfur_out$bounds, check_bounds, tolerance = 1e-5)

            out1 <- capture.output(darfur_out)
            out1 <- capture.output(summary(darfur_out))
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            darfur_out2 <- sensemakr(model3, treatment = "directlyharmed")
            plot(darfur_out2)
            ovb_contour_plot(darfur_out2)
            ovb_extreme_plot(darfur_out2)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))

          })

test_that(desc = "testing darfur sensemakr but negative",
          {


            darfur_out <- sensemakr(model4, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot(darfur_out)
            ovb_contour_plot(darfur_out)
            ovb_extreme_plot(darfur_out)

            # info
            expect_equal(darfur_out$info$formula, peacefactor ~ directlyharmed + farmer_dar + herder_dar +
                           pastvoted + hhsize_darfur + female | village + age)
            expect_equal(darfur_out$info$treatment, "directlyharmed")
            expect_equal(darfur_out$info$q, 1)
            expect_equal(darfur_out$info$alpha, 0.05)
            expect_equal(darfur_out$info$reduce, TRUE)

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, 485)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02760859, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.1549014, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.0761147, tolerance = 1e-5)

            # bounds
            check_bounds <- structure(list(bound_label = c("1x female", "2x female", "3x female"),
                                           r2dz.x = c(0.009586695, 0.019173391, 0.028760086),
                                           r2yz.dx = c(0.1520541, 0.3041646, 0.4563329),
                                           treatment = rep("directlyharmed", 3),
                                           adjusted_estimate = c(-0.07057237, -0.04956097, -0.02833820),
                                           adjusted_se = c(0.02280808, 0.02076204, 0.01844235),
                                           adjusted_t = c(-3.094183, -2.387096, -1.536583),
                                           adjusted_lower_CI = -1*c(0.11538722, 0.09035563, 0.06457496),
                                           adjusted_upper_CI = -1*c(0.025757524, 0.008766321,-0.007898566)
            ),
            .Names = c("bound_label", "r2dz.x", "r2yz.dx", "treatment",
                       "adjusted_estimate", "adjusted_se", "adjusted_t",
                       "adjusted_lower_CI", "adjusted_upper_CI"),
            row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equivalent(darfur_out$bounds, check_bounds, tolerance = 1e-5)

            out1 <- capture.output(darfur_out)
            out1 <- capture.output(summary(darfur_out))
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            darfur_out2 <- sensemakr(model4, treatment = "directlyharmed")
            plot(darfur_out2)
            ovb_contour_plot(darfur_out2)
            ovb_extreme_plot(darfur_out2)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))

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

            model.treat <- fixest::feols(directlyharmed ~ farmer_dar + herder_dar +
                                pastvoted + hhsize_darfur + female | village + age, data = darfur)

            darfur_out <- sensemakr(estimate = 0.09137704,
                                    se = 0.02462435,
                                    dof = 485,
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    r2dxj.x = partial_r2(model.treat, covariates = "female"),
                                    r2yxj.dx = partial_r2(model2, covariates = "female"),
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

            # sensitivity stats
            expect_equal(darfur_out$sensitivity_stats$dof, 485)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02760858, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.1549013, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.07611466, tolerance = 1e-5)

            # bounds
            check_bounds <-
              structure(
                list(
                  bound_label = c("1x female", "2x female", "3x female"),
                  r2dz.x = c(0.009586695, 0.019173391, 0.028760086),
                  r2yz.dx = c(0.1520541, 0.3041646, 0.4563329),
                  adjusted_estimate = c(0.07057237, 0.04956097, 0.02833820),
                  adjusted_se = c(0.02280808, 0.02076204, 0.01844235),
                  adjusted_t = c(3.094183, 2.387096, 1.536583),
                  adjusted_lower_CI = c(0.025757524, 0.008766321, -0.007898566),
                  adjusted_upper_CI = c(0.11538722, 0.09035563, 0.06457496)
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
            rv <- robustness_value(model2, covariates = "directlyharmed")
            expect_equivalent(c(rv), 0.1549014, tolerance = 1e-5)
            expect_equivalent(attributes(rv)$q, 1)
            expect_equivalent(attributes(rv)$names, "directlyharmed")

            ## RV q = 1, alpha = 0.05
            rv <- robustness_value(model2, covariates = "directlyharmed", q = 1, alpha = 0.05)
            expect_equivalent(c(rv), 0.0761147, tolerance = 1e-5)
            expect_equivalent(attributes(rv)$q, 1)
            expect_equivalent(attributes(rv)$alpha, 0.05)
            expect_equivalent(attributes(rv)$names, "directlyharmed")

            # checks partial R2
            r2 <- partial_r2(model2, covariates = "directlyharmed")
            expect_equivalent(r2, 0.02760859, tolerance = 1e-5)

            # checks partial f2
            f2 <- partial_f2(model2, covariates = "directlyharmed")
            expect_equivalent(f2,  0.02839246, tolerance = 1e-5)

            # sensitivity stats
            sens_stats <- sensitivity_stats(model2, treatment = "directlyharmed")
            expect_equivalent(sens_stats$treatment, "directlyharmed")
            expect_equivalent(sens_stats$estimate, 0.09137704, tolerance = 1e5)
            expect_equivalent(sens_stats$se, 0.02462435, tolerance = 1e5)
            expect_equivalent(sens_stats$t_statistic, 3.710842, tolerance = 1e5)
            expect_equivalent(sens_stats$r2yd.x, 0.02760859, tolerance = 1e5)
            expect_equivalent(sens_stats$rv_q , 0.1549014, tolerance = 1e5)
            expect_equivalent(sens_stats$rv_qa , 0.0761147, tolerance = 1e5)
            expect_equivalent(sens_stats$f2yd.x , 0.02839246, tolerance = 1e5)
            expect_equivalent(sens_stats$dof , 485, tolerance = 1e5)

            expect_equivalent(group_partial_r2(model2, covariates = "directlyharmed"), partial_r2(model2, covariates = "directlyharmed"), tolerance = 1e5)
            expect_error(group_partial_r2(model2))
            expect_equal(group_partial_r2(model2, covariates = c("directlyharmed", "female")), 0.1388258, tolerance = 1e-5)

          })

test_that(desc = "testing darfur adjusted estimates",
          {

            should_be_zero <- adjusted_estimate(model2, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model2, covariates = "directlyharmed"))
            expect_equivalent(should_be_zero, 0)

            rv <- robustness_value(model2, covariates = "directlyharmed")
            should_be_zero <- adjusted_estimate(model2, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_zero, 0)


            rv <- robustness_value(model2, covariates = "directlyharmed", alpha = 0.05)
            thr <- qt(0.975, df = 485 - 1)
            should_be_1.96 <- adjusted_t(model2, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(c(should_be_1.96), thr)

            should_be_estimate <- bias(model2, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model2, covariates = "directlyharmed"))
            expect_equivalent(should_be_estimate, coef(model2)["directlyharmed"])

            rv <- robustness_value(model2, covariates = "directlyharmed")
            should_be_estimate <- bias(model2, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_estimate, coef(model2)["directlyharmed"])

            rv <- robustness_value(model2, covariates = "directlyharmed", q = 0.5)
            should_be_half_estimate <- bias(model2, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
            expect_equivalent(should_be_half_estimate, coef(model2)["directlyharmed"]/2)

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
            expect_equivalent(bounds$adjusted_estimate, c(0.03681659, 0.02471622, 0.01258421), tolerance = 1e-5)
            expect_equivalent(c(bounds$adjusted_t), c(2.1440119, 1.5601124, 0.8749287), tolerance = 1e-5)

            # test extreme scenario plot
            extreme_out <- ovb_extreme_plot(model2, treatment = "directlyharmed", kd = 1:3)
            adj_est <- adjusted_estimate(model2, treatment = "directlyharmed",
                                         r2yz.dx = 1,
                                         r2dz.x = extreme_out$scenario_r2yz.dx_1$r2dz.x[5])
            expect_equivalent(adj_est, extreme_out$scenario_r2yz.dx_1$adjusted_estimate[5])
          })

test_that("testing darfur print",
          {
            skip_on_cran()
            darfur_out <- sensemakr(model2, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            darfur_out2 <- sensemakr(model2, treatment = "directlyharmed")




            print.sense <- "Sensitivity Analysis to Unobserved Confounding\n\nModel Formula: peacefactor ~ directlyharmed + farmer_dar + herder_dar + pastvoted + \n    hhsize_darfur + female | village + age\n\nNull hypothesis: q = 1 and reduce = TRUE \n\nUnadjusted Estimates of ' directlyharmed ':\n  Coef. estimate: 0.09138 \n  Standard Error: 0.02462 \n  t-value: 3.71084 \n\nSensitivity Statistics:\n  Partial R2 of treatment with outcome: 0.02761 \n  Robustness Value, q = 1 : 0.1549 \n  Robustness Value, q = 1 alpha = 0.05 : 0.07611 \n\nFor more information, check summary."
            compare <- capture_output(print(darfur_out))
            expect_equal(compare, print.sense)




            summary.sense <- "Sensitivity Analysis to Unobserved Confounding\n\nModel Formula: peacefactor ~ directlyharmed + farmer_dar + herder_dar + pastvoted + \n    hhsize_darfur + female | village + age\n\nNull hypothesis: q = 1 and reduce = TRUE \n-- This means we are considering biases that reduce the absolute value of the current estimate.\n-- The null hypothesis deemed problematic is H0:tau = 0 \n\nUnadjusted Estimates of 'directlyharmed': \n  Coef. estimate: 0.0914 \n  Standard Error: 0.0246 \n  t-value (H0:tau = 0): 3.7108 \n\nSensitivity Statistics:\n  Partial R2 of treatment with outcome: 0.0276 \n  Robustness Value, q = 1: 0.1549 \n  Robustness Value, q = 1, alpha = 0.05: 0.0761 \n\nVerbal interpretation of sensitivity statistics:\n\n-- Partial R2 of the treatment with the outcome: an extreme confounder (orthogonal to the covariates) that explains 100% of the residual variance of the outcome, would need to explain at least 2.76% of the residual variance of the treatment to fully account for the observed estimated effect.\n\n-- Robustness Value, q = 1: unobserved confounders (orthogonal to the covariates) that explain more than 15.49% of the residual variance of both the treatment and the outcome are strong enough to bring the point estimate to 0 (a bias of 100% of the original estimate). Conversely, unobserved confounders that do not explain more than 15.49% of the residual variance of both the treatment and the outcome are not strong enough to bring the point estimate to 0.\n\n-- Robustness Value, q = 1, alpha = 0.05: unobserved confounders (orthogonal to the covariates) that explain more than 7.61% of the residual variance of both the treatment and the outcome are strong enough to bring the estimate to a range where it is no longer 'statistically different' from 0 (a bias of 100% of the original estimate), at the significance level of alpha = 0.05. Conversely, unobserved confounders that do not explain more than 7.61% of the residual variance of both the treatment and the outcome are not strong enough to bring the estimate to a range where it is no longer 'statistically different' from 0, at the significance level of alpha = 0.05.\n\nBounds on omitted variable bias:\n\n--The table below shows the maximum strength of unobserved confounders with association with the treatment and the outcome bounded by a multiple of the observed explanatory power of the chosen benchmark covariate(s).\n\n Bound Label R2dz.x R2yz.dx      Treatment Adjusted Estimate Adjusted Se\n   1x female 0.0096  0.1521 directlyharmed            0.0706      0.0228\n   2x female 0.0192  0.3042 directlyharmed            0.0496      0.0208\n   3x female 0.0288  0.4563 directlyharmed            0.0283      0.0184\n Adjusted T Adjusted Lower CI Adjusted Upper CI\n     3.0942            0.0258            0.1154\n     2.3871            0.0088            0.0904\n     1.5366           -0.0079            0.0646"
            compare <- capture_output(summary(darfur_out))
            expect_equal(compare, summary.sense)





            latex.table <- c("\\begin{table}[!h]", "\\centering", "\\begin{tabular}{lrrrrrr}",
                             "\\multicolumn{7}{c}{Outcome: \\textit{peacefactor}} \\\\", "\\hline \\hline ",
                             "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ & $RV_{q = 1}$ & $RV_{q = 1, \\alpha = 0.05}$  \\\\ ",
                             "\\hline ", "\\textit{directlyharmed} & 0.091 & 0.025 & 3.711 & 2.8\\% & 15.5\\% & 7.6\\% \\\\ ",
                             "\\hline ", "df = 485 & & \\multicolumn{5}{r}{ \\small \\textit{Bound (1x female)}: $R^2_{Y\\sim Z| {\\bf X}, D}$ = 15.2\\%, $R^2_{D\\sim Z| {\\bf X} }$ = 1\\%} \\\\",
                             "\\end{tabular}", "\\end{table}")
            expect_equal(capture.output(ovb_minimal_reporting(darfur_out)), latex.table)

            latex2 <- c("\\begin{table}[!h]", "\\centering", "\\begin{tabular}{lrrrrrr}",
                        "\\multicolumn{7}{c}{Outcome: \\textit{peacefactor}} \\\\", "\\hline \\hline ",
                        "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ & $RV_{q = 1}$ & $RV_{q = 1, \\alpha = 0.05}$  \\\\ ",
                        "\\hline ", "\\textit{directlyharmed} & 0.091 & 0.025 & 3.711 & 2.8\\% & 15.5\\% & 7.6\\% \\\\ ",
                        "\\hline ", "df = 485 & & \\multicolumn{5}{r}{ }\\end{tabular}",
                        "\\end{table}")
            expect_equal(capture.output(ovb_minimal_reporting(darfur_out2)), latex2)

          })


test_that("testing darfur different q",
          {
            darfur_out <- sensemakr(model2, treatment = "directlyharmed", benchmark_covariates = "female", q = 2, kd = 1:3)
            rvq <- darfur_out$sensitivity_stats$rv_q
            rvqa <- darfur_out$sensitivity_stats$rv_qa
            expect_equivalent(rvq, robustness_value(model2, covariates = "directlyharmed", q = 2))
            expect_equivalent(rvqa, robustness_value(model2, covariates = "directlyharmed", q = 2,alpha = 0.05))
          })

test_that("Darfur group benchmarks",
          {
            village <- grep(pattern = "village", names(coef(model5)), value = T)

            sensitivity <- sensemakr(model5, treatment = "directlyharmed",
                                     benchmark_covariates = list(village = village),
                                     kd = 0.3)

            r2y <- group_partial_r2(model5, covariates = village)
            treat.model <- update(model5, directlyharmed ~ .-directlyharmed)
            r2d <- group_partial_r2(treat.model, covariates = village)
            bounds.check <- ovb_partial_r2_bound(r2dxj.x = r2d, r2yxj.dx = r2y, kd = 0.3)
            bounds <- sensitivity$bounds
            expect_equal(bounds$r2dz.x, bounds.check$r2dz.x)
            expect_equal(bounds$r2yz.dx, bounds.check$r2yz.dx)

          })
