library (eurobarometer)
library(testthat)
context("Correct class conversion to 5 non-negative levels")

test_that("correct conversion takes place", {
  expect_equal(as_factor_5( c("DK", "Much lower", "Somewhat lower","More or less the same", 	"Somewhat higher", "Much higher")
)[[3]],
  c( NA, 0, 1, 2, 3, 4 )
  )
})

test_that("class factor_pos_neg is recognized", {
  expect_equal(is.factor_5(
    as_factor_5( c("DK", "Much lower", "Somewhat lower",
                   "More or less the same",
                   "Somewhat higher", "Much higher")
                 )),
               TRUE
  )
})


test_that("correct as_numeric method", {
  expect_equal(as_numeric(
    as_factor_5( c("DK", "Much lower", "Somewhat lower","More or less the same", 	"Somewhat higher", "Much higher")
    )),
    c( NA, 0, 1, 2, 3, 4 )
  )
})
