#' Get naming exceptions
#'
#' These names are not changed automatically with regex rules.
#' @examples
#' get_naming_exceptions()
#' @export

get_naming_exceptions <- function () {
  naming_exceptions <- data.frame (
    exact = c("HOUSEHOLD COMPOSITION: AGED <10",
              "COMPOSITION: AGED 10-14",
              "HOUSEHOLD COMPOSITION: AGED 15+",
              "DIFFICULTIES PAYING BILLS - LAST YEAR",
              "DIFFICULTIES PAYING BILLS",
              "TYPE OF COMMUNITY",
              "WEIGHT RESULT FROM TARGET (REDRESSMENT)",
              "WEIGHT RESULT FROM TARGET",
              "W1 WEIGHT RESULT FROM TARGET",
              "WEIGHT EXTRAPOLATED POPULATION AGED 15+",
              "WEIGHT EXTRAPOLATED POPULATION AGED 15",
              "WEX WEIGHT EXTRA POPULATION 15+",
              "WEIGHT EXTRA POPULATION 15+",
              "WEIGHT EXTRA POPULATION 15",
              "WEIGHT EXTRAPOLATED POPULATION 15+",
              "WEIGHT EXTRAPOLATED POPULATION 15",
              "NATION - ALL SAMPLES ISO 3166",
              "ALL SAMPLES ISO 3166"
    ),
    new_name = c("household_composition_10m",
                 "household_composition_aged_10_14",
                 "household_composition_aged_15p",
                 "difficulties_bill",
                 "difficulties_bill",
                 "subjective_urbanization",
                 "w1", "w1","w1",
                 "wex", "wex", "wex", "wex", "wex",
                 "wex", "wex",
                 "country_code_iso_3166",
                 "country_code_iso_3166"),
    stringsAsFactors = FALSE
  )
  return (naming_exceptions)
}



