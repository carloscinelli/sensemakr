context("Testing Darfur Example")
# runs regression model

data("darfur")

model2 <-  lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village,
              data = within(darfur,  directlyharmed <- directlyharmed*(-1)))

model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)

test_that("testing darfur data", {


  expect_equal(dim(darfur), c(1276, 14))

  expect_equal(colnames(darfur),
               c("wouldvote",
                 "peacefactor",
                 "peace_formerenemies",
                 "peace_jjindiv",
                 "peace_jjtribes",
                 "gos_soldier_execute",
                 "directlyharmed",
                 "age",
                 "farmer_dar",
                 "herder_dar",
                 "pastvoted",
                 "hhsize_darfur",
                 "village",
                 "female"))
})

test_that(desc = "testing darfur sensemakr",
          {
            darfur_out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot(darfur_out)
            ovb_contour_plot(darfur_out)
            ovb_extreme_plot(darfur_out)

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
                                           adjusted_estimate = c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548),
                                           adjusted_se = c(0.0218733277437572, 0.0203500620779637, 0.0186700648170924),
                                           adjusted_t = c(3.43890386024675, 2.60024623913809, 1.62806202131271),
                                           adjusted_lower_CI = c(0.032282966, 0.012968035,-0.006253282),
                                           adjusted_upper_CI = c(0.11815758, 0.09286231, 0.06704533)
                                           ),
                                      .Names = c("bound_label", "r2dz.x", "r2yz.dx",
                                                 "adjusted_estimate", "adjusted_se", "adjusted_t",
                                                 "adjusted_lower_CI", "adjusted_upper_CI"),
                                      row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equal(darfur_out$bounds, check_bounds)

            out1 <- capture.output(darfur_out)
            out1 <- capture.output(summary(darfur_out))
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            darfur_out2 <- sensemakr(model, treatment = "directlyharmed")
            plot(darfur_out2)
            ovb_contour_plot(darfur_out2)
            ovb_extreme_plot(darfur_out2)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))

          })


test_that(desc = "testing darfur sensemakr but negative",
          {


            darfur_out <- sensemakr(model2, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            plot(darfur_out)
            ovb_contour_plot(darfur_out)
            ovb_extreme_plot(darfur_out)

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
                                           adjusted_estimate = c(-0.0752202712144491, -0.0529151723844518, -0.0303960234641548),
                                           adjusted_se = c(0.0218733277437572, 0.0203500620779637, 0.0186700648170924),
                                           adjusted_t = c(-3.43890386024675, -2.60024623913809, -1.62806202131271),
                                           adjusted_lower_CI = -1*c(0.11815758, 0.09286231, 0.06704533),
                                           adjusted_upper_CI = -1*c(0.032282966, 0.012968035,-0.006253282)
                                           ),
                                      .Names = c("bound_label", "r2dz.x", "r2yz.dx",
                                                 "adjusted_estimate", "adjusted_se", "adjusted_t",
                                                 "adjusted_lower_CI", "adjusted_upper_CI"),
                                      row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equal(darfur_out$bounds, check_bounds)

            out1 <- capture.output(darfur_out)
            out1 <- capture.output(summary(darfur_out))
            out3 <- capture.output(ovb_minimal_reporting(darfur_out))
            darfur_out2 <- sensemakr(model, treatment = "directlyharmed")
            plot(darfur_out2)
            ovb_contour_plot(darfur_out2)
            ovb_extreme_plot(darfur_out2)
            out3 <- capture.output(ovb_minimal_reporting(darfur_out2))

          })

test_that(desc = "testing darfur sensemakr with formula",
          {
            darfur_out <- sensemakr(formula = peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                                      pastvoted + hhsize_darfur + female + village, data = darfur, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)

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
                                           adjusted_estimate = c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548),
                                           adjusted_se = c(0.0218733277437572, 0.0203500620779637, 0.0186700648170924),
                                           adjusted_t = c(3.43890386024675, 2.60024623913809, 1.62806202131271),
                                           adjusted_lower_CI = c(0.032282966, 0.012968035,-0.006253282),
                                           adjusted_upper_CI = c(0.11815758, 0.09286231, 0.06704533)),
                                      .Names = c("bound_label", "r2dz.x", "r2yz.dx",
                                                 "adjusted_estimate", "adjusted_se", "adjusted_t",
                                                 "adjusted_lower_CI", "adjusted_upper_CI"),
                                      row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equal(darfur_out$bounds, check_bounds)
          })


test_that(desc = "testing darfur sensemakr manually",
          {

            model.treat <- lm(directlyharmed ~  age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

            darfur_out <- sensemakr(estimate = 0.09731582,
                                    se = 0.02325654,
                                    dof = 783,
                                    treatment = "directlyharmed",
                                    benchmark_covariates = "female",
                                    r2dxj.x = partial_r2(model.treat, covariates = "female"),
                                    r2yxj.x = partial_r2(model, covariates = "female"),
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
            expect_equal(darfur_out$sensitivity_stats$dof, 783)
            expect_equal(darfur_out$sensitivity_stats$r2yd.x, 0.02187, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_q), 0.13878, tolerance = 1e-5)
            expect_equivalent(c(darfur_out$sensitivity_stats$rv_qa), 0.07626, tolerance = 1e-5)

            # bounds
            check_bounds <- structure(list(bound_label = c("1x female", "2x female", "3x female"),
                                           r2dz.x = c(0.00916428667504862, 0.0183285733500972, 0.0274928600251459),
                                           r2yz.dx = c(0.12464092303637, 0.249324064199975, 0.374050471038094),
                                           adjusted_estimate = c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548),
                                           adjusted_se = c(0.0218733277437572, 0.0203500620779637, 0.0186700648170924),
                                           adjusted_t = c(3.43890386024675, 2.60024623913809, 1.62806202131271)),
                                      .Names = c("bound_label", "r2dz.x", "r2yz.dx",
                                                 "adjusted_estimate", "adjusted_se", "adjusted_t"),
                                      row.names = c(NA, -3L), class = c("ovb_bounds", "data.frame"))

            expect_equal(as.data.frame(darfur_out$bounds), as.data.frame(check_bounds[1:3]))
          })


test_that(desc = "testing darfur sensitivity stats",{

            # checks RV
            ## RV q = 1
            rv <- robustness_value(model, covariates = "directlyharmed")
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

            expect_equivalent(group_partial_r2(model, covariates = "directlyharmed"), partial_r2(model, covariates = "directlyharmed"))
            expect_error(group_partial_r2(model))
            expect_equal(group_partial_r2(model, covariates = c("directlyharmed", "female")), 0.1350435, tolerance = 1e-5)

})

test_that(desc = "testing darfur adjusted estimates",{

  should_be_zero <- adjusted_estimate(model, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model, covariates = "directlyharmed"))
  expect_equivalent(should_be_zero, 0)

  rv <- robustness_value(model, covariates = "directlyharmed")
  should_be_zero <- adjusted_estimate(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
  expect_equivalent(should_be_zero, 0)


  rv <- robustness_value(model, covariates = "directlyharmed", alpha = 0.05)
  thr <- qt(0.975, df = 783 - 1)
  should_be_1.96 <- adjusted_t(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
  expect_equivalent(c(should_be_1.96), thr)

  should_be_estimate <- bias(model, treatment = "directlyharmed", r2yz.dx = 1, r2dz.x = partial_r2(model, covariates = "directlyharmed"))
  expect_equivalent(should_be_estimate, coef(model)["directlyharmed"])

  rv <- robustness_value(model, covariates = "directlyharmed")
  should_be_estimate <- bias(model, treatment = "directlyharmed", r2yz.dx = rv, r2dz.x = rv)
  expect_equivalent(should_be_estimate, coef(model)["directlyharmed"])

  rv <- robustness_value(model, covariates = "directlyharmed", q = 0.5)
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
            expect_equivalent(bounds$adjusted_estimate, c(0.0752202712144491, 0.0529151723844518, 0.0303960234641548))
            expect_equivalent(bounds$adjusted_t, c(3.43890386024675, 2.60024623913809, 1.62806202131271))

            # test extreme scenario plot
            extreme_out <- ovb_extreme_plot(model, treatment = "directlyharmed", kd = 1:3)
            adj_est <- adjusted_estimate(model, treatment = "directlyharmed",
                                         r2yz.dx = 1,
                                         r2dz.x = extreme_out$scenario_r2yz.dx_1$r2dz.x[5])
            expect_equivalent(adj_est, extreme_out$scenario_r2yz.dx_1$adjusted_estimate[5])
}

)


test_that("testing darfur print",
          {
            skip_on_cran()
            darfur_out <- sensemakr(model, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
            darfur_out2 <- sensemakr(model, treatment = "directlyharmed")

            print.sense <- "Sensitivity Analysis to Unobserved Confounding\n\nModel Formula: peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + \n    pastvoted + hhsize_darfur + female + village\n\nUnadjusted Estimates of ' directlyharmed ':\n  Coef. estimate: 0.09732 \n  Standard Error: 0.02326 \n  t-value: 4.18445 \n\nSensitivity Statistics:\n  Partial R2 of treatment with outcome: 0.02187 \n  Robustness Value, q = 1 : 0.13878 \n  Robustness Value, q = 1 alpha = 0.05 : 0.07626 \n\nFor more information, check summary."
            compare <- capture_output(print(darfur_out))
            expect_equal(compare, print.sense)

            summary.sense <- "Sensitivity Analysis to Unobserved Confounding\n\nModel Formula: peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + \n    pastvoted + hhsize_darfur + female + village\n\nUnadjusted Estimates of 'directlyharmed': \n  Coef. estimate: 0.0973 \n  Standard Error: 0.0233 \n  t-value: 4.1844 \n\nSensitivity Statistics:\n  Partial R2 of treatment with outcome: 0.0219 \n  Robustness Value, q = 1: 0.1388 \n  Robustness Value, q = 1, alpha = 0.05: 0.0763 \n\nVerbal interpretation of sensitivity statistics:\n\nUnobserved confounders (orthogonal to the covariates) that explain more than 13.88% of the residual variance of both the treatment and the outcome are enough to reduce the absolute value of the effect size by 100%. Conversely, unobserved confounders that do not explain more than 13.88% of the residual variance of both the treatment and the outcome are not strong enough to reduce the absolute value of the effect size by 100% .\n\nUnobserved confounders (orthogonal to the covariates) that explain more than 7.63% of the residual variance of both the treatment and the outcome are enough to reduce the absolute value of the effect size by 100 % at the significance level of alpha = 0.05 . Conversely, unobserved confounders that do not explain more than 7.63% of the residual variance of both the treatment and the outcome are not strong enough to reduce the absolute value of the effect size by 100% at the significance level of alpha = 0.05 .\n\nAn extreme confounder (orthogonal to the covariates) that explains 100% of the residual variance of the outcome, would need to explain at least 2.19% of the residual variance of the treatment to fully account for the observed estimated effect.\n\nOVB bounds:\n Bound Label R2dz.x R2yz.dx Adjusted Estimate Adjusted Se Adjusted T\n   1x female 0.0092  0.1246            0.0752      0.0219     3.4389\n   2x female 0.0183  0.2493            0.0529      0.0204     2.6002\n   3x female 0.0275  0.3741            0.0304      0.0187     1.6281\n Adjusted Lower CI Adjusted Upper CI\n            0.0323            0.1182\n            0.0130            0.0929\n           -0.0063            0.0670"
            compare <- capture_output(summary(darfur_out))
            expect_equal(compare, summary.sense)

            latex.table <- c("\\begin{table}[!h]", "\\centering", "\\begin{tabular}{lrrrrrr}",
                             "\\multicolumn{7}{c}{Outcome: \\textit{peacefactor}} \\\\", "\\hline \\hline ",
                             "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ & $RV_{q = 1}$ & $RV_{q = 1, \\alpha = 0.05}$  \\\\ ",
                             "\\hline ", "\\textit{directlyharmed} & 0.097 & 0.023 & 4.184 & 2.2\\% & 13.9\\% & 7.6\\% \\\\ ",
                             "\\hline ", "df = 783 & & \\multicolumn{5}{r}{ \\small \\textit{Bound (1x female)}: $R^2_{Y\\sim Z| {\\bf X}, D}$ = 12.5\\%, $R^2_{D\\sim Z| {\\bf X} }$ = 0.9\\%} \\\\",
                             "\\end{tabular}", "\\end{table}")
            expect_equal(capture.output(ovb_minimal_reporting(darfur_out)), latex.table)

            latex2 <- c("\\begin{table}[!h]", "\\centering", "\\begin{tabular}{lrrrrrr}",
                        "\\multicolumn{7}{c}{Outcome: \\textit{peacefactor}} \\\\", "\\hline \\hline ",
                        "Treatment: & Est. & S.E. & t-value & $R^2_{Y \\sim D |{\\bf X}}$ & $RV_{q = 1}$ & $RV_{q = 1, \\alpha = 0.05}$  \\\\ ",
                        "\\hline ", "\\textit{directlyharmed} & 0.097 & 0.023 & 4.184 & 2.2\\% & 13.9\\% & 7.6\\% \\\\ ",
                        "\\hline ", "df = 783 & & \\multicolumn{5}{r}{ }\\end{tabular}",
                        "\\end{table}")
            expect_equal(capture.output(ovb_minimal_reporting(darfur_out2)), latex2)

          })
