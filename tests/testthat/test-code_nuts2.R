library (eurobarometer)
library (testthat)
context("Concerting NUTS2 names to NUTS2 codes")

#country_code <- za5929$country_code
#region_nuts_codes <- za5929$region_nuts_codes

test_that("small countries are projected", {
  expect_equal(code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA),
                            country_code = c("AT", "CZ", "CY")),
              c("AT33", "CZ01", "CY00")
  )
})

test_that("small countries are omitted", {
  expect_equal(code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA)),
               c("AT33", "CZ01", NA)
  )
})

test_that("error message displayed", {
  expect_equal(code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA)),
               c("AT33", "CZ01", NA),
               nuts_code = "code_2006"
  )
})



