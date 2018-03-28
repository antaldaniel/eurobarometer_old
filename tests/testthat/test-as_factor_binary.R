library (eurobarometer)
library(testthat)
context("Correct class conversion to binary levels")

test_that("correct conversion takes place", {
  expect_equal(as_factor_binary( c("mentioned", "mentioned",
                              "not mentioned", "dk"))[[3]],
  c( 1,1,0,NA )
  )
})

test_that("class factor_binary is recognized", {
  expect_equal(is.factor_binary(
    as_factor_binary( c("mentioned", "mentioned",
                   "not mentioned", "dk")
                 )),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(
    as_factor_binary( c("mentioned", "mentioned",
                   "not mentioned", "dk")
    )),
    c( 1,1,0,NA )
  )
})

test_that("correct error message", {
  expect_error(as_factor_binary( c("london", "paris",
                        "amsterdam", "dk")))
})
