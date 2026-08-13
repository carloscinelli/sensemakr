context("Non-iid vcov note for fixest models")

# fixest is Suggests-only: R CMD check runs the test suite with a library
# containing only the test framework, so this file must not error there.
if (requireNamespace("fixest", quietly = TRUE)) {

data("darfur")

fml <- peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
  pastvoted + hhsize_darfur + female + village

feols.iid <- fixest::feols(fml, data = darfur)
feols.hc1 <- fixest::feols(fml, data = darfur, vcov = "hetero")

# =========================================================================
# The note must reach the entry points users actually call.
# These two paths had no coverage, which is why a silent regression in
# message_vcov.fixest() went unnoticed on 8 of 9 CI jobs.
# =========================================================================

test_that("sensemakr.fixest emits the note for a non-iid vcov", {
  expect_message(sensemakr(feols.hc1, treatment = "directlyharmed"), "iid")
})

test_that("sensitivity_stats.fixest emits the note for a non-iid vcov", {
  expect_message(sensitivity_stats(feols.hc1, treatment = "directlyharmed"), "iid")
  expect_silent(sensitivity_stats(feols.hc1, treatment = "directlyharmed",
                                  message = FALSE))
})

test_that("no note is emitted for an iid model", {
  expect_silent(sensemakr(feols.iid, treatment = "directlyharmed"))
  expect_silent(sensitivity_stats(feols.iid, treatment = "directlyharmed"))
  expect_silent(robustness_value(feols.iid, covariates = "directlyharmed"))
  expect_silent(adjusted_se(feols.iid, treatment = "directlyharmed",
                            r2dz.x = 0.05, r2yz.dx = 0.05))
})

# =========================================================================
# The note is derived from what fixest actually computed, not from the
# unevaluated call. Passing vcov through a variable, as the formula methods
# do internally, must not be mistaken for a non-iid vcov.
# =========================================================================

test_that("a vcov argument passed as a variable is not read as non-iid", {
  build <- function(formula, data, vcov = "iid") {
    fixest::feols(fml = formula, data = data, vcov = vcov)
  }
  model <- build(fml, darfur)

  # the call holds the symbol `vcov`, not the string it evaluated to
  expect_true(is.symbol(model$call$vcov))

  expect_silent(adjusted_se(model, treatment = "directlyharmed",
                            r2dz.x = 0.05, r2yz.dx = 0.05))
  expect_silent(adjusted_t(model, treatment = "directlyharmed",
                           r2dz.x = 0.05, r2yz.dx = 0.05))
  expect_silent(adjusted_ci(model, treatment = "directlyharmed",
                            r2dz.x = 0.05, r2yz.dx = 0.05))
})

# =========================================================================
# ovb_bounds computes both an adjusted se and an adjusted t. The note
# belongs to the model, not to each statistic, so it must appear once.
# =========================================================================

count_notes <- function(expr) {
  notes <- character()
  withCallingHandlers(
    expr,
    message = function(m) {
      notes <<- c(notes, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  sum(grepl("iid", notes))
}

test_that("the note is emitted once per call, not once per statistic", {
  expect_equal(count_notes(
    ovb_bounds(feols.hc1, treatment = "directlyharmed",
               benchmark_covariates = "female", kd = 1)), 1)

  expect_equal(count_notes(
    ovb_extreme_plot(feols.hc1, treatment = "directlyharmed",
                     benchmark_covariates = "female", kd = 1)), 1)

  expect_equal(count_notes({
    ovb_contour_plot(feols.hc1, treatment = "directlyharmed")
    add_bound_to_contour(feols.hc1, treatment = "directlyharmed",
                         benchmark_covariates = "female", kd = 1)
  }), 1)

  expect_silent(ovb_bounds(feols.hc1, treatment = "directlyharmed",
                           benchmark_covariates = "female", kd = 1,
                           message = FALSE))
})

}
