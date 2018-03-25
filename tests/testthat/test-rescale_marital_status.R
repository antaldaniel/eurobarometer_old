library (eurobarometer)
library(testthat)
context("Wrapper for Eurobarometer marital status in English")

test_that("correct conversion takes place", {
  expect_equal(rescale_marital_status (
    column = c("Widow/er: living without children",
             "(Re-)Married: w children of this marriage",
             "DK"),
    na_labels = "DK",
    return_class = "character",
    underscore = FALSE),
  c( "widow no children", "married own children", NA )
  )
})


test_that("No numeric return is allowed", {
  expect_error(rescale_marital_status (c("Widow/er: living without children",
                                         "(Re-)Married: w children of this marriage",
                                         "DK"),
                                       return_class = "numeric",
                                       underscore = FALSE)
  )
})

