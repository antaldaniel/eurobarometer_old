library (eurobarometer)
library(testthat)
context("Correct class conversion to positive neutral negative")

x <- c("Better", "DK", "Worse",
       "Same", "The Same", "Inap. not")

test_that("correct conversion takes place", {
  expect_equal(as_factor_pos_neg (c("Better", "DK", "Worse",
                                    "Same", "The Same",
                                    "Inap. not "))[[3]],
  c( 1, NA, -1, 0, 0,NA )
  )
})

test_that("class factor_pos_neg is recognized", {
  expect_equal(is.factor_pos_neg(as_factor_pos_neg (c("Better", "DK", "Worse",
                                    "Same", "The Same",
                                    "Inap. not "))),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(as_factor_pos_neg (c("Better", "DK", "Worse",
                                    "Same", "The Same",
                                    "Inap. not "))),
               c( 1, NA, -1, 0, 0,NA )
  )
})
