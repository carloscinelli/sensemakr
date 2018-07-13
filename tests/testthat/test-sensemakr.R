context("test-sensemakr.R")

test_that("load darfur data", {
  data(darfur)

  expect_equal(dim(darfur), c(1276, 14))

  expect_equal(colnames(darfur),
               c("wouldvote", "peacefactor",
                 "FormerEnemiesPeace",
                 "PeaceWithJJIndiv",
                 "PeaceWithJJTribes",
                 "GoSsoldier_execute",
                 "directlyharmed",
                 "age",
                 "farmer_dar",
                 "herder_dar",
                 "pastvoted",
                 "hhsize_darfur",
                 "village",
                 "female"))
})

test_that("check paper calculations", {
  expect_equal(test_obj$dof, 783)
  expect_equal(unname(test_obj$r2_yd), 0.02187, tolerance = 1e-3)
  expect_equal(unname(test_obj$rv), 0.13878, tolerance = 1e-3)
  expect_equal(unname(test_obj$rv_t), 0.07626, tolerance = 1e-3)

  mock_bench = structure(
    list(variable = "female", bound = 0.0220955480705063,
         r2y = 0.109033915427841, r2d = 0.00908106519032963),
    row.names = 1L, class = "data.frame")

  expect_equal(test_obj$benchmark, mock_bench, tolerance = 1e-3)
})

test_that("verbosity", {
  # To test verbosity
  sensemakr(formula = peacefactor ~ directlyharmed + age +
              farmer_dar + herder_dar + pastvoted + hhsize_darfur +
              female + village,
            treatment = "directlyharmed",
            data = darfur,
            benchmark = "female",
            verbose = TRUE)
})

test_that("do plots", {
  # Plot
  plot(test_obj)
  contour_plot(test_obj)
  plot(test_obj, plot_t = TRUE)

  # OVB plot test
  ovb_plot(model = test_obj$model_outcome,
           covariate = test_obj$treatment_variable)

  # And again, with the treatment effect directly.
  ovb_plot(estimate = test_obj$treatment_effect)

})

test_that("no benchmark variable", {
  data(darfur)
  no_bench = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur)
  expect_null(no_bench$benchmark)

  plot(no_bench)
})

test_that("invalid arguments to sensemakr", {
  data(darfur)
  expect_error(sensemakr(formula = DV ~ IV1 + IV2,
                         data = darfur,
                         treatment = "IV1"))
  expect_error(sensemakr(formula = peacefactor ~ directlyharmed,
                         data = darfur,
                         treatment = "directlyharmed"))

  junk_subset = darfur[darfur$peacefactor == 2, ]
  expect_error(sensemakr(formula = peacefactor ~ directlyharmed + age +
                           farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                           female + village,
                         treatment = "directlyharmed",
                         data = junk_subset,
                         verbose = TRUE))

  expect_error(sensemakr(formula = peacefactor ~ directlyharmed + age + female,
                         data = darfur,
                         treatment = "directlyharmed",
                         benchmark = "invalid"))
})

test_that("printing", {
  # These are not great tests
  no_fe = sensemakr(formula = peacefactor ~ directlyharmed + age +
                      farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                      female ,
                    treatment = "directlyharmed",
                    benchmark = "female",
                    data = darfur)
  print(no_fe, print_covariates = TRUE)
  print(no_fe, print_covariates = TRUE, sort_by = "alpha")
  print(no_fe, print_covariates = TRUE, sort_by = "R2Y")
  print(no_fe, print_covariates = TRUE, sort_by = "R2D")

  # LaTeX output
  make_table(test_obj,
             outcome_label = "override1",
             treatment_label = "override2",
             benchmark_label = "override3",
             caption = "caption goes here",
             label = "fig:rv")

})

test_that("data not in a df", {
  skip("test_that global assignment not working")
  # Hack to test data in the global environment; testthat doesn't like doing
  # this, but going to force global assignment to try to make it work.

  X = rnorm(100)
  e = rnorm(100)
  D = c(rep(1, 50), rep(0, 50))
  Y = 3*D + 0.3 * X + e

  test_create_object = sensemakr(
    formula = Y ~ D + X,
    treatment = "D",
    benchmark = "X"
  )

  expect_error(sensemakr(
    formula = Y ~ D + X,
    treatment = "Z"
  ))

  expect_error(sensemakr(
    formula = Y ~ D + X,
    treatment = "D",
    benchmark = "Z"
  ))

})
