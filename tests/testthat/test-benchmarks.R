context("test-benchmarks.R")

test_that("add and remove benchmarks", {
  data(darfur)

  no_bench = sensemakr(formula = peacefactor ~ directlyharmed + age +
                         farmer_dar + herder_dar + pastvoted + hhsize_darfur +
                         female + village,
                       treatment = "directlyharmed",
                       data = darfur)

  # Add a few benchmarks
  no_bench = add_benchmark(no_bench, "female")
  expect_equal(nrow(no_bench$benchmark), 1)
  no_bench = add_benchmark(no_bench, "pastvoted")
  expect_equal(nrow(no_bench$benchmark), 2)

  # Take advance of this test to test plotting with benchmark override
  plot(no_bench, benchmark_covariate = "female", main = "Test Main Title")

  # Verify the benches added properly.
  mock_bench = structure(list(variable = c("female", "pastvoted"),
                              bound = c(0.0220955480705063,
                                        0.00179811118358916),
                              r2y = c(0.109033915427841,
                                      0.00506841252461435),
                              r2d = c(0.00908106519032963,
                                      0.00148974060290356)),
                         row.names = c(NA, -2L), class = "data.frame")
  expect_equal(mock_bench, no_bench$benchmark, tolerance = 1e-3)

  # Add invalid benches
  expect_error(add_benchmark(no_bench, "invalid"))
  expect_error(add_benchmark(no_bench, "directlyharmed"))
  expect_error(add_benchmark(no_bench, NULL))

  # Verify we can remove benches
  no_bench = remove_benchmarks(no_bench)
  expect_null(no_bench$benchmark)

  # Table with no benchmarks
  make_table(no_bench)
})

test_that("misc tests for non-sensemakr objects with sensemakr methods", {
  expect_error(add_benchmark("hello world", "hello"))
  expect_error(remove_benchmarks("hello world"))
})
