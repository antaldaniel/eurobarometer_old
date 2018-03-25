library (eurobarometer)
library(testthat)
context("Correct class conversion to positive neutral negative")


as_factor_yes_no_4( c("Very good", "Rather Good",
                      "Inap.", "Rather Bad", "DK",
                      "Very bad"))
test_that("correct conversion takes place", {
  expect_equal(as_factor_yes_no_4 (c("Very good", "Rather Good",
                                     "Inap.", "Rather Bad", "DK",
                                     "Very bad"))[[3]],
  c( -2, -1, NA, 1, NA, 2 )
  )
})

test_that("class factor_pos_neg is recognized", {
  expect_equal(is.factor_yes_no_4(
     as_factor_yes_no_4(c("Very good", "Rather Good",
                                   "Inap.", "Rather Bad", "DK",
                                   "Very bad"))),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(as_factor_yes_no_4 (c("Very good", "Rather Good",
                                                "Inap.", "Rather Bad", "DK",
                                                "Very bad"))),
               c( -2, -1, NA, 1, NA, 2 )
  )
})
