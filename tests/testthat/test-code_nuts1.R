library (eurobarometer)
library (testthat)
context("Concerting NUTS1 names to NUTS1 codes")

#country_code <- za5929$country_code
#region_nuts_codes <- za5929$region_nuts_codes

test_that("small countries are projected", {
  expect_equal(code_nuts1  (
    region_nuts_codes = c("Eesti", "Republic of Cyprus",
                          "Malta", "Luxembourg")
    ),
    c("EE0", "CY0", "MT0", "LU0")
  )
  expect_equal(code_nuts1  (
    country_code = c("EE", "CY", "MT", "LU"),
    region_nuts_codes = c("Eesti", "Republic of Cyprus",
                          "Malta", "Luxembourg")
  ),
  c("EE0", "CY0", "MT0", "LU0")
  )
})



