library (eurobarometer)
library(testthat)
context("Converting categorical values to numeric values.")

test_that("correct conversion takes place", {
  expect_equal(rescale_categories (column = c("Agree", "Disagree")),
    c(1,0)
  )
  expect_equal(rescale_categories (column = c("Agree",
                                              "Disagree", NA),
                                   na_labels = NA),
               c(1,0,NA)
  )
  expect_equal(rescale_categories (column =
           c("Agree", "AGREE", "disagree",
              "DK", "Refusal (SPONT.)", "Disagree"),
           na_labels = c("DK", "Refusal (SPONT.)")),
           c(1,1,0,NA,NA,0)
  )
})

context("Converting categorical values to character values.")

test_that("correct conversion takes place", {
  expect_equal(rescale_categories (column = c("Agree", "Disagree"),
                                   return_class = "character"),
               c("1","0")
  )
  expect_equal(rescale_categories (column =
                                     c("Agree", "AGREE", "disagree",
                                       "DK", "Refusal (SPONT.)", "Disagree"),
                                   na_labels = c("DK", "Refusal (SPONT.)"),
                                   return_class = "character"),
               c("1","1","0",NA,NA,"0")
  )
})

context("Converting categorical values to factor values.")


test_that("correct conversion takes place", {
  expect_equal(levels(rescale_categories (column = c("Agree", "Disagree"),
                                   return_class = "factor")),
               c("0","1")
  )
  expect_equal(rescale_categories (column =
                                     c("Agree", "AGREE", "disagree",
                                       "DK", "Refusal (SPONT.)", "Disagree"),
                                   na_labels = c("DK", "Refusal (SPONT.)"),
                                   return_class = "factor"),
               as.factor( c("1", "1", "0", NA, NA, "0"))
  )
})

context("Converting categorical values to factor values with missing values")

test_that("default NA id works", {
  expect_equal(rescale_categories (column = c("Agree", "Disagree", "NA", "DK"),
                                   na_labels = "default",
                                   return_class = "numeric"),
               c(1, 0, NA, NA)
  )
  expect_equal(rescale_categories(
    c( "Szinte soha / Soha", "Az esetek többségében", "NA"),
    from = c("szinte", "az esetek", "néha"),
    to = c(0,2,1),
    exact_from = FALSE,
    na_labels = "NA"
  ),
  c(0,2,NA))
})


context("Creating meaningful errors and messages")

test_that("mismatch in inputs", {
  expect_error(rescale_categories (column = c("Agree", "Disagree", "NA", "DK"),
                                   from =c("agree", "tend to agree", "disagree"),
                                   to = c(1,0),
                                   na_labels = "default",
                                   return_class = "numeric"))
  expect_error(rescale_categories (column = NULL,
                                   from =c("agree", "tend to agree", "disagree"),
                                   to = c(1,0),
                                   na_labels = "default",
                                   return_class = "numeric"))
})
