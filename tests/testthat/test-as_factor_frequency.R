library (eurobarometer)
library(testthat)
context("Correct class conversion to 4 non-negative levels")
as_factor_frequency( x  = c("Less often", "Several times a month",
                       "Every 2-3 months", "Once a month", "DK"))

test_that("correct conversion takes place", {
  expect_equal(as_factor_frequency( x  = c("Less often", "Several times a month",
                                           "Every 2-3 months", "Once a month", "DK"))[[3]],
  c( 3,36,6,12,NA )
  )
})
