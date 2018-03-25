library (eurobarometer)
library(testthat)
context("Correct class conversion to 4 non-negative levels")


test_that("correct conversion takes place", {
  expect_equal(as_factor_4( c("Not at all", "Low",
                              "Medium", "Strong", "DK"))[[3]],
  c( 0,1,2,3,NA )
  )
})

test_that("correct conversion takes place", {
  expect_equal(as_factor_4(c("Not at all likely", "Very likely",
                             "Fairly likely", "DK",
                             "Not very likely"))[[3]],
               c( 0,3,2,NA,1 )
  )
})

test_that("class factor_pos_neg is recognized", {
  expect_equal(is.factor_4(
    as_factor_4( c("Not at all", "Low",
                   "Medium", "Strong", "DK"))),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(as_factor_4( c("Not at all", "Low",
                                         "Medium", "Strong", "DK"))),
               c( 0,1,2,3,NA )
  )
})
