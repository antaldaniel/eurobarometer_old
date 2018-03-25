library (surveyreader)
library(testthat)
context("Wrapper for time frequency of using things Eurobarometer questions.")

test_that("correct conversion takes place", {
  expect_equal(rescale_time_frequency_use (column = c(
    "Everyday/Almost everyday", "Less often", "never",
    "NEVER", "NA", "DK", "No internet access"),
    na_labels = "default"),
    c(250,12,0,0,NA,NA,0)
  )
})

