context("Testing plots")

test_that("Plot warnings and errors",
          {
            data("darfur")

            # runs regression model
            model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                          pastvoted + hhsize_darfur + female + village, data = darfur)

            expect_warning(ovb_contour_plot(model, treatment = "directlyharmed", lim = 2))
            expect_warning(ovb_contour_plot(model, treatment = "directlyharmed", lim = -1))
            expect_warning(ovb_extreme_plot(model, treatment = "directlyharmed", lim = 2))
            expect_warning(ovb_extreme_plot(model, treatment = "directlyharmed", lim = -1))



            expect_warning(ovb_contour_plot(estimate = 2, se =2, dof = 100,  lim = 2))
            expect_warning(ovb_contour_plot(estimate = 2, se =2, dof = 100,  lim = -1))
            expect_error(ovb_contour_plot.numeric(estimate = NULL, se =2, dof = 100,  lim = -1))
            expect_error(ovb_contour_plot.numeric(estimate = "a", se =2, dof = 100,  lim = -1))

            expect_error(ovb_contour_plot(model, treatment = 2))
            expect_error(ovb_contour_plot(model, treatment = c("a", "b")))
            ovb_contour_plot(model, treatment = "directlyharmed", r2dz.x = .1)

            expect_error(ovb_contour_plot(model, treatment = "directlyharmed", list.par = "a"))

            expect_error(ovb_contour_plot(y ~ x, treatment = "x", data = darfur))
            df <- data.frame(x = NULL, y = NULL)
            expect_error(ovb_contour_plot(peacefactor ~ directlyharmed ,
                                          treatment = "directlyharmed",
                                          data = "abc"))
            ovb_contour_plot(model, treatment = "directlyharmed", list.par = NULL)
            out <- ovb_bounds(model, treatment = "directlyharmed", benchmark_covariates = "female")
            add_bound_to_contour(out)

            expect_error(ovb_extreme_plot(model, treatment = 2))
            expect_error(ovb_extreme_plot(model, treatment = c("a", "b")))

            expect_warning(ovb_extreme_plot.numeric(estimate = 1, se =2, dof = 100, lim = -1))
            expect_warning(ovb_extreme_plot.numeric(estimate = 1, se =2, dof = 100, lim = 1))
            expect_error(ovb_extreme_plot.numeric(estimate = 1, se =2, dof = 100, lim = .2, list.par = "b"))
            expect_warning(ovb_contour_plot(model, treatment = "directlyharmed", lim = -1))

          })
