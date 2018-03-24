#' Code gender variable
#'
#' Create a gender indicator or dummy from a character vector.
#' @param column A column from a survey data frame where gender is recorded.
#' @param female_id A regex identifier for the female values. If omitted,
#' the default value is \code{"default"} searches for \code{"woman"} or
#' \code{"female"} (not case-sensitive).
#' @param female_value What should be the returned value for females (Default: \code{1} )
#' @param na_id  Is there a special character ID for missing variables? (Default: \code{"NA"} )
#' Missing variables will be always treated as missing, however, if you have a special NA identifier,
#' It will also be treated as missing. Every other case will be treated "male"
#' @param return_class Default is \code{"numeric"}, alternatives \code{"character"} or
#' \code{"factor"}.
#' @importFrom haven as_factor
#' @examples
#' code_gender(column = as.character (c("1-no", "1-nő",
#'                                      "NA", NA, "0")),
#'             female_id = "1", female_value = 1,
#'             na_id = "NA")
#' @export

recode_gender <- function ( column,
                          female_id = "default",
                          female_value = 1,
                          na_id = "NA",
                          return_class = "numeric") {
  if ( ! return_class %in% c("numeric", "factor", "character")) {
    stop("Only numeric, factor or character type returns are allowed")
  }
  if ( "labelled" %in% class(column)) {
    column <- haven::as_factor (column) }

  column <- tolower(as.character (column) )
  if (female_id == "default") {
    possible_female_id = c("female", "woman")
    female_id <- possible_female_id [which (
         possible_female_id %in% column)]
    message ("Female id: '", female_id, "'.")
  }

  if (return_class == "numeric") {
    if (! female_value %in% c(0,1)) stop("Female_value must be 0 or 1.")

    if (female_value == 1) {
      male_value <- 0 } else {
        male_value <- 1 }

    if (! is.na(na_id) ) {
      column <- ifelse ( column == na_id, NA, column )
    }

    column <- ifelse ( grepl(female_id, tolower(column)),
                       yes = female_value,
                       no =  column )

    column <- ifelse ( column %in% c(female_value, NA),
                       column, male_value)

    return(as.numeric(column))
  } #end of numeric case

  if ( return_class == "character" ) return(as.character(column))
  if ( return_class == "numeric" ) return(as.numeric(as.character(column)))
  if ( return_class == "factor" ) return(as.factor(as.character(column)))

}
