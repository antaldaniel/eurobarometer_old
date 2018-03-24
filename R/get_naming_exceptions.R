#' Get naming exceptions
#'
#' These names are not changed automatically with regex rules.
#' @examples
#' get_naming_exceptions
#' @export

get_naming_exceptions <- function () {
  naming_exceptions <- data.frame (
    exact = c("HOUSEHOLD COMPOSITION: AGED <10",
              "COMPOSITION: AGED 10-14",
              "HOUSEHOLD COMPOSITION: AGED 15+",
              "DIFFICULTIES PAYING BILLS - LAST YEAR",
              "TYPE OF COMMUNITY",
              "WEIGHT RESULT FROM TARGET (REDRESSMENT)",
              "WEIGHT RESULT FROM TARGET",
              "WEIGHT EXTRAPOLATED POPULATION AGED 15+"
    ),
    new_name = c("household_composition_10m",
                 "household_composition_aged_10_14",
                 "household_composition_aged_15p",
                 "difficulties_bill",
                 "subjective_urbanization",
                 "w1", "w1",
                 "wex"),
    stringsAsFactors = FALSE
  )
  return (naming_exceptions)
}



