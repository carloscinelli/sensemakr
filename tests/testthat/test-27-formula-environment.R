context("The formula interface works away from the global environment")

data("darfur")

fml <- peacefactor ~ directlyharmed + age + female

null_device <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

# =========================================================================
# The formula methods used to rebuild the lm() call out of substitute() and
# evaluate it with no environment, which resolves in the method's own frame
# and from there only along the lexical chain to the global environment. Any
# data frame living in a calling frame was therefore invisible.
# =========================================================================

test_that("sensemakr.formula finds a data frame passed as an argument", {
  f <- function(dat) {
    sensemakr(fml, data = dat, method = "lm", treatment = "directlyharmed")
  }
  expect_s3_class(f(darfur), "sensemakr")
})

test_that("sensemakr.formula finds a data frame held in a local variable", {
  g <- function() {
    d <- darfur
    sensemakr(fml, data = d, method = "lm", treatment = "directlyharmed")
  }
  expect_s3_class(g(), "sensemakr")
})

test_that("the data frame is found through nesting and through lapply", {
  one <- function(dat) sensemakr(fml, data = dat, method = "lm",
                                 treatment = "directlyharmed")
  two <- function(dat) one(dat)
  expect_s3_class(two(darfur), "sensemakr")

  out <- lapply(list(darfur), function(dat)
    sensemakr(fml, data = dat, method = "lm", treatment = "directlyharmed"))
  expect_s3_class(out[[1]], "sensemakr")
})

test_that("a formula built in a calling frame is found", {
  k <- function() {
    local_fml <- peacefactor ~ directlyharmed + age + female
    sensemakr(local_fml, data = darfur, method = "lm",
              treatment = "directlyharmed")
  }
  expect_s3_class(k(), "sensemakr")
})

test_that("both plot formula methods work from inside a function", {
  cp <- function(dat) ovb_contour_plot(formula = fml, data = dat, method = "lm",
                                       treatment = "directlyharmed",
                                       benchmark_covariates = "female", kd = 1)
  ep <- function(dat) ovb_extreme_plot(formula = fml, data = dat, method = "lm",
                                       treatment = "directlyharmed",
                                       benchmark_covariates = "female", kd = 1)
  expect_silent(null_device(cp(darfur)))
  expect_silent(null_device(ep(darfur)))
})

# =========================================================================
# The formula interface must agree with the model interface exactly, and
# must keep doing so from inside a function.
# =========================================================================

test_that("benchmark bounds match the lm interface exactly", {
  model <- lm(fml, data = darfur)
  ref <- ovb_bounds(model, treatment = "directlyharmed",
                    benchmark_covariates = c("female", "age"), kd = c(1, 2, 3))

  top <- sensemakr(fml, data = darfur, method = "lm",
                   treatment = "directlyharmed",
                   benchmark_covariates = c("female", "age"), kd = c(1, 2, 3))
  expect_equal(as.data.frame(ref), as.data.frame(top$bounds), tolerance = 0)

  inside <- function(dat) {
    sensemakr(fml, data = dat, method = "lm", treatment = "directlyharmed",
              benchmark_covariates = c("female", "age"), kd = c(1, 2, 3))
  }
  expect_equal(as.data.frame(ref), as.data.frame(inside(darfur)$bounds),
               tolerance = 0)
})

test_that("group benchmark bounds match the lm interface exactly", {
  model <- lm(fml, data = darfur)
  grp <- list(grp = c("female", "age"))
  ref <- ovb_bounds(model, treatment = "directlyharmed",
                    benchmark_covariates = grp, kd = 2)
  out <- sensemakr(fml, data = darfur, method = "lm",
                   treatment = "directlyharmed",
                   benchmark_covariates = grp, kd = 2)
  expect_equal(as.data.frame(ref), as.data.frame(out$bounds), tolerance = 0)
})

# =========================================================================
# The returned object no longer carries the method's own frame, and with it
# the fitted model and a copy of the data, through the stored formula.
# =========================================================================

test_that("the returned object does not retain the method's internals", {
  s <- sensemakr(peacefactor ~ directlyharmed + age + female, data = darfur,
                 method = "lm", treatment = "directlyharmed")
  env <- environment(s$info$formula)
  expect_false(any(c("outcome_model", "reg.call") %in% ls(env)))
})
