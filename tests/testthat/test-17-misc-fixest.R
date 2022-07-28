context("test-misc fixest")


test_that("weird regression model with r^2",
          {
  mean = rnorm(10)
  error = rnorm(10, sd = 0.3)
  y = mean + error
  constant = rep(2, 10)
  group = as.factor(c(rep(1:5, 2)))
  factor = as.factor(1:10)

  # multicol_model = lm(y ~ group + constant)
  # expect_warning(partial_r2(multicol_model))

  data = model.matrix(y ~ factor)[,-1]
  data = cbind(y, data)
  fml <- reformulate(paste(colnames(data)[-1], collapse = " + "), response = "y")
  saturated_model = fixest::feols(fml = fml, data = data)
  expect_error(partial_r2(saturated_model))
})
