#' Charcter to numeric conversion
#'
#' Convert a character to a numeric expression.
#' @param column A column from a survey data frame where gender is recorded.
#' @param validate_min The valid minimum value. Defaults to \code{NA} in which case no validation takes place.
#' @param validate_max The valid maximum value.  Defaults to \code{NA} in which case no validation takes place.
#' @param digits How many digits are valid in the raw date. Defaults to \code{NA} when
#' digits are not truncated. \code{2} uses only the first two characters of the raw data.
#' Digits are counted as characters. So keeping 1.5 requires \code{digit = 3}.
#' @param decimals Defaults to \code{"point"}, alternative is \code{"comma"}
#' @param na_id  Is there a special character ID for missing variables? (Default: \code{"NA"} )
#' @param is_integer Numeric or integer should be returned?  Defaults to \code{FALSE} in which case no validation takes place.
#' If you get a warning with NA conversion, you are likely to have several NA characters,
#' such as \code{missing} and \code{NA} at the same time.
#' @importFrom stringr str_sub
#' @examples
#' chr_numeric(column = as.character (c(1982, 1990.5, 2019,
#'                                      "missing", NA, 1678)),
#'             validate_min  = 1900, validate_max = 2017,
#'             digits = NA, decimals = "point",
#'             na_id = "missing",
#'             is_integer = FALSE )
#' chr_numeric(column = as.character (c("1 - egyet", "11 - tizenegyet",
#'             "missing", "2")),
#'             validate_min  = NA, validate_max = NA,
#'             digits = 2,
#'             na_id = "missing",
#'             is_integer = FALSE )
#' @export

chr_numeric <- function ( column,
                          validate_min = NA,
                          validate_max = NA,
                          digits = NA,
                          decimals = "point",
                          na_id = "NA",
                          is_integer = FALSE) {
  if ( !is.na(na_id)) {
    column <- ifelse ( grepl(na_id, column),
                       yes = NA, no = column )
  } #fist NAs because with digits = 2 "missing" = "mi"

  column <- as.character ( column )
  if ( !is.na(digits)) {
    column <- stringr::str_sub ( column, 1, digits )
  }

  if (!decimals %in% c("point", "comma")) {
    warning("Decimals are understood with points, ", decimals,
            "is not a valid parameter,")
    decimals <- "point"
  }
  column <- gsub("[[:space:]]", "", column ) #remove space

  if (decimals == "point") {
    column <- gsub("[^0-9\\.]", "", column) #keep only numbers
  } else if ( decimals == "comma" ) {
    column <- gsub("[^0-9\\,]", "", column)
    column <- gsub("\\,", ".", column)
  }

  column <- as.numeric (column)

  if(!is.na(validate_min)) {
    column <- ifelse (column < validate_min,
                      yes = NA, column)
  }
  if(!is.na(validate_max)) {
    column <- ifelse (column > validate_max,
                      yes = NA, column)
  }

  if ( is_integer == TRUE) {
    return(as.integer(column))
  }
  return(as.numeric(column))

}
