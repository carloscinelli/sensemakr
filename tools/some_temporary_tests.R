f <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
  pastvoted + hhsize_darfur + female + village
lm.out  <- lm(f, data = darfur)
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
partial_r2("wow")

robustness_value(t_statistic = 4, dof = 762)
robustness_value(lm.out, covariate = c("directlyharmed","female"))
rv(t_statistic = 4, dof = 762, alpha = 0.05)
coef(lm.out)

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
             threshold = -0.3, lim = 0.15)

ovb_extreme_plot(estimate = 0.09731582,se = 0.02325654, dof = 783, r2dz = 0.1, reduce = FALSE,
             threshold = 0.3)

names(darfur)
test <- ovb_contour_plot(lm.out, treatment = "directlyharmed")
str(test)

test <- ovb_contour_plot(lm.out, treatment = "directlyharmed", type = "t-value")
str(test)

test2 <- ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
str(test2)

test2 <- ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3,
                          type = "t-value")
str(test2)


test3 <- ovb_extreme_plot(lm.out, treatment = "directlyharmed")
str(test3)

test4 <- ovb_extreme_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = "female", kd = 1:3)
str(test4)


ovb_contour_plot(lm.out, treatment = "directlyharmed",
                 r2dz.x = 0.2, r2yz.dx = 0.2, bound_label = "my bound",
                 benchmark_covariates = "female", kd = 1:3)

undebug(ovb_contour_plot)
ovb_contour_plot(f, data = darfur, treatment = "directlyharmed",
                 r2dz.x = 0.2, r2yz.dx = 0.2, bound_label = "my bound",
                 benchmark_covariates = "female", kd = 1:3)


ovb_extreme_plot(f, data = darfur, treatment = "directlyharmed",
                 benchmark_covariates = "female", kd = 1:3, lim = 0.05)

ovb_extreme_plot(lm.out, treatment = "directlyharmed",
                 benchmark_covariates = "female", kd = 1:3, lim = 0.05)


ovb_contour_plot(lm.out, treatment = "directlyharmed", benchmark_covariates = c("female", "age"), kd = 1:3)

ovb_add_bound_plot(lm.out, treatment = "directlyharmed", benchmark_covariate = "female",
                   kd = 20, ky = 2, text_label = F)

ovb_contour_plot(lm.out2, treatment = "I(-directlyharmed)")

ovb_partial_r2_bound(lm.out,treatment = "directlyharmed", benchmark_covariate = c("female", "age"),
                     kd = 1:3)

plot_benchmark(r2yx = 0.1, r2dx = 0.01,
               covariate_name = "female",
               multipliers_y = 1, multipliers_d = 1,
               estimate = 0.4,se = 0.2, dof = 765, plot_t = F)

min(c(NULL, 0.1))

bound_calculator(0.1, 0.01)
table <- ovb_table(lm.out,treatment = "directlyharmed")
str(table$rv_q)
table$rv_qa
table$rv_qa
table$rv_q


summary(Indometh)
wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time", direction = "wide")
wide
str(wide)
reshape(wide, direction = "long")
reshape(test$grid, varying = rownames(test$grid),
        direction = "long")
str(test$grid)

debug(group_partial_r2)
group_partial_r2(lm.out, covariates = c("female", "village"))
partial_r2(lm.out, covariates = "female")
ovb_bounds(lm.out, treatment = "directlyharmed",
           benchmark_covariates = "female", kd = 1)

sensitivity_stats(lm.out, treatment = "directlyharmed")

ovb_bounds(lm.out, treatment = "directlyharmed",
           benchmark_covariates = c("female","age"), kd = 1:3)
