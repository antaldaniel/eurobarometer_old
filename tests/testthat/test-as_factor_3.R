library (eurobarometer)
library(testthat)
context("Correct class conversion to 3 non-negative levels")


test_that("correct conversion takes place", {
  expect_equal(as_factor_3( c("Rural area or village", "Small or medium-sized town",
                              "Inap.", "Large town/city", "DK", "Large town/city"))[[3]],
  c( 0, 1, NA, 2, NA, 2 )
  )
})

test_that("class factor_pos_neg is recognized", {
  expect_equal(is.factor_3(
    as_factor_3( c("Rural area or village", "Small or medium-sized town",
                   "Inap.", "Large town/city", "DK", "Large town/city"))),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(as_factor_3( c("Rural area or village", "Small or medium-sized town",
                                         "Inap.", "Large town/city", "DK", "Large town/city"))),
               c( 0, 1, NA, 2, NA, 2 )
  )
})
