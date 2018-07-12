context("test-ovb.R")

test_that("traditional ovb plot", {
  # Missing everything
  expect_error(ovb_plot())

  # Nonnumeric estimate
  expect_error(ovb_plot(estimate = "hello"))

  # Misspecified lim
  expect_error(ovb_plot(estimate = 2,
                          lim = c(1, 1, 1, 1)))
  expect_error(ovb_plot(estimate = 2,
                          lim = c(1, 0, 0.2)))
  expect_error(ovb_plot(estimate = 2,
                          lim = c(-1, 2, 0.3)))

  ovb_plot(estimate = 2)

  test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur)
  ovb_plot(test_obj)

  plot(test_obj, type="ovb")
})
