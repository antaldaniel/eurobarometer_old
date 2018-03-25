library (eurobarometer)
library(testthat)
context("Wrapper for Eurobarometer social class self-placement in English")

test_that("correct conversion takes place", {
  expect_equal(rescale_occupation (c("Owner of a shop, craftsmen, etc.",
                                     "Retired, unable to work",
                                     "Employed position, at desk",
                                     "Employed professional (employed doctor, etc.)",
                                     "DK"),
                                   return_class = "character"),
  c( "shop-owner", "retired", "employed_office",
     "employed_professional",NA )
  )
  expect_equal(rescale_occupation (c("Owner of a shop, craftsmen, etc.",
                                     "Retired, unable to work",
                                     "Employed position, at desk",
                                     "Employed professional (employed doctor, etc.)",
                                     "DK"),
                                   underscore = FALSE),
               factor(c("shop-owner", "retired", "employed office",
                        "employed professional", NA),
                      levels = c("employed office", "employed professional", "retired",
                                 "shop-owner", NA))
  )
  expect_equal(rescale_occupation (c("Owner of a shop, craftsmen, etc.",
                                     "Retired, unable to work",
                                     "Employed position, at desk",
                                     "Employed professional (employed doctor, etc.)",
                                     "DK"),
                                   return_class = "numeric"),
  c(24,14,34,31,NA)
  )
})


