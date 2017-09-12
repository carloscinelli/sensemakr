context("Checking vignette for sensemakr")

test_that("Testing vignette load'", {

  #######################################
  # does not build vignette
  #######################################
  # devtools::install_github("chadhazlett/sensemakr")

  library(sensemakr)


  # vignette(all=TRUE)
  # vignette(package = "sensemakr")

  #######################################
  # this does build vignette
  #######################################
  # devtools::install_github("chadhazlett/sensemakr",build_vignettes = TRUE,force=TRUE)
  table_vign = vignette(all=TRUE)
  lgl_sense_vign = 'sensemakr' %in% (table_vign$results)[,'Package']
  expect_that(lgl_sense_vign, is_true())

  out_vign = vignette('sensemakr',package = "sensemakr")
  expect_that(out_vign, is_a("vignette"))

  })
