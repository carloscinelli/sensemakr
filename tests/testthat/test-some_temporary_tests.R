context("Playground for coding to be formalized into tests")

lm.out  <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)
lm.out2  <- lm(peacefactor ~ I(-directlyharmed) + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)




partial_r2(lm.out)[1:2]
partial_r2(lm.out, covariate = "directlyharmed")
partial_r2(4, 762)
partial_f2(lm.out)[1:2]
partial_f(lm.out)[1:2]
partial_r2(4, 100)
partial_f2(4, 100)
partial_f(4, 100)


robustness_value(t_statistic = 4, dof = 762)
robustness_value(lm.out, covariate = c("directlyharmed","female"))
robustness_value(lm.out, covariate = c("directlyharmed","female"), q = 0.5, alpha = 0.05)


ovb_contour_plot(estimate = 0.09731582,se = 0.02325654, dof = 783)

ovb_contour_plot(estimate = 0.09731582,se = 0.02325654, dof = 783, reduce = FALSE)
ovb_contour_plot(estimate = -0.09731582,se = 0.02325654, dof = 783)



ovb_extreme_plot(estimate = 0.09731582,se = 0.02325654, dof = 783, lim = 0.01)
ovb_extreme_plot(estimate = 0.09731582,se = 0.02325654, dof = 783, r2dz = 0.1, lim = 0.3)
ovb_extreme_plot(estimate = -0.09731582,se = 0.02325654, dof = 783, r2dz = 0.1)

ovb_extreme_plot(lm.out, treatment = "directlyharmed", r2dz = 0.1)
ovb_extreme_plot(estimate = 0.09731582,se = 0.02325654, dof = 783,  r2dz = 0.1)
ovb_extreme_plot(lm.out, treatment = "directlyharmed", benchmark_covariate = "female")
ovb_extreme_plot(lm.out, treatment = "directlyharmed", benchmark_covariate = "female", kd = 1:3)

ovb_extreme_plot(lm.out, treatment = "directlyharmed", benchmark_covariate = "female",
                 cex.legend = 0.5, lim = 0.15)
ovb_extreme_plot(estimate = -0.09731582,se = 0.02325654, dof = 783, r2dz = 0.1, reduce = TRUE,
                 threshold = -0.03, lim = 0.15)

ovb_extreme_plot(estimate = 0.09731582,se = 0.02325654, dof = 783, r2dz = 0.1, reduce = FALSE,
             threshold = 0.3)

test <- ovb_contour_plot(lm.out, treatment = "directlyharmed")
str(test)

test <- ovb_contour_plot(lm.out, treatment = "directlyharmed", sensitivity.of = "t-value")
str(test)

test2 <- ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
str(test2)

test2 <- ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3,
                          sensitivity.of = "t-value")
str(test2)


test3 <- ovb_extreme_plot(lm.out, treatment = "directlyharmed")
str(test3)

test4 <- ovb_extreme_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
str(test4)


ovb_contour_plot(lm.out, treatment = "directlyharmed",
                 r2dz.x = 0.2, r2yz.dx = 0.2, bound_label = "my bound",
                 benchmark_covariates = "female", kd = 1:3)

ovb_contour_plot(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                   pastvoted + hhsize_darfur + female + village, data = darfur, treatment = "directlyharmed",
                 r2dz.x = 0.2, r2yz.dx = 0.2, bound_label = "my bound",
                 benchmark_covariates = "female", kd = 1:3)


ovb_extreme_plot(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                   pastvoted + hhsize_darfur + female + village, data = darfur, treatment = "directlyharmed",
                 benchmark_covariates = "female", kd = 1:3, lim = 0.05)

ovb_extreme_plot(lm.out, treatment = "directlyharmed",
                 benchmark_covariates = "female", kd = 1:3, lim = 0.05)


ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = c("female", "age"), kd = 1:3)

add_bound_to_contour(lm.out, treatment = "directlyharmed", benchmark_covariate = "female",
                     kd = 20, ky = 2)

ovb_contour_plot(lm.out2, treatment = "I(-directlyharmed)")

ovb_partial_r2_bound(lm.out,treatment = "directlyharmed", benchmark_covariate = c("female", "age"),
                     kd = 1:3)
ovb_bounds(lm.out, treatment = "directlyharmed", benchmark_covariates = "female")
ovb_bounds(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", adjusted_estimates = F)


table <- sensitivity_stats(lm.out,treatment = "directlyharmed")
str(table$rv_q)
table$rv_q
table$rv_qa



group_partial_r2(lm.out, covariates = "female")
partial_r2(lm.out, covariates = "female")
group_partial_r2(lm.out, covariates = c("female", "age"))

ovb_bounds(lm.out, treatment = "directlyharmed",
           benchmark_covariates = "female", kd = 1:5)

ovb_bounds(lm.out, treatment = "directlyharmed",
           benchmark_covariates = c("female","age"), kd = 1:3)


sense <- sensemakr(lm.out, treatment = "directlyharmed")
plot(sense)

sense <- sensemakr(lm.out, treatment = "directlyharmed", benchmark_covariates = "female",
                   kd = 1:3)
plot(sense)
plot(sense, type = "extreme")
sense$info


sense <- sensemakr(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                     pastvoted + hhsize_darfur + female + village,
                   data = darfur,
                   treatment = "directlyharmed", benchmark_covariates = "female",
                   kd = 1:3)

sense
sense$info
sense$sensitivity_stats$treatment

ovb_minimal_reporting(sense)
