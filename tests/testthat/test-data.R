context("Checking data")

test_that("Darfur data is correct", {
  data("darfur")
  vars <- c("wouldvote", "peacefactor", "FormerEnemiesPeace", "PeaceWithJJIndiv",
            "PeaceWithJJTribes", "GoSsoldier_execute", "directlyharmed",
            "age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur",
            "village", "female")
  expect_true(all(vars %in% names(darfur)))
  expect_equal(nrow(darfur), 1276)

})
