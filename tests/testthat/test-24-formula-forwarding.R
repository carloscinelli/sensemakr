context("formula method forwards its arguments")

test_that("ovb_contour_plot.formula honours r2yz.dx", {
  # The .formula method used to forward `r2yz.dx = r2dz.x`, so a user-supplied
  # r2yz.dx was silently replaced by r2dz.x and the bound was drawn at the
  # wrong point on the contour.
  #
  # The formula is written inline rather than held in a variable because
  # ovb_contour_plot.formula() substitutes and evaluates it in the parent
  # frame, which does not see test-local bindings.
  data("darfur")

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  from_lm <- ovb_contour_plot(
    lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
         pastvoted + hhsize_darfur + female + village, data = darfur),
    treatment = "directlyharmed",
    r2dz.x = 0.05, r2yz.dx = 0.30,
    bound_label = "manual")

  from_fml <- ovb_contour_plot(
    peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
      pastvoted + hhsize_darfur + female + village,
    method = "lm", data = darfur,
    treatment = "directlyharmed",
    r2dz.x = 0.05, r2yz.dx = 0.30,
    bound_label = "manual")

  expect_equal(from_fml$bounds$r2dz.x, 0.05)
  expect_equal(from_fml$bounds$r2yz.dx, 0.30)
  expect_equal(from_fml$bounds[, c("r2dz.x", "r2yz.dx")],
               from_lm$bounds[, c("r2dz.x", "r2yz.dx")])
})
