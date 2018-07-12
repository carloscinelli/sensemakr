context("test-benchmarks.R")

test_that("add and remove benchmarks", {
  data(darfur)

  test_obj = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur)

  test_obj = add_benchmark(test_obj, "female")
  expect_equal(nrow(test_obj$benchmark), 1)
  test_obj = add_benchmark(test_obj, "pastvoted")
  expect_equal(nrow(test_obj$benchmark), 2)

  mock_bench = structure(list(variable = c("female", "pastvoted"),
                              bound = c(0.0220955480705063,
                                        0.00179811118358916),
                              r2y = c(0.109033915427841,
                                      0.00506841252461435),
                              r2d = c(0.00908106519032963,
                                      0.00148974060290356)),
                         row.names = c(NA, -2L), class = "data.frame")

  expect_equal(mock_bench, test_obj$benchmark, tolerance = 1e-3)
  expect_error(add_benchmark(test_obj, "invalid"))
  expect_error(add_benchmark(test_obj, "directlyharmed"))
  expect_error(add_benchmark(test_obj, NULL))

  test_obj = remove_benchmarks(test_obj)
  expect_null(test_obj$benchmark)
})

test_that("misc tests for non-sensemakr objects with sensemakr methods", {
  expect_error(add_benchmark("hello world", "hello"))
  expect_error(remove_benchmarks("hello world"))
})
