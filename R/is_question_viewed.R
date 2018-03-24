#' Is question viewed?
#'
#' This is a (malformatted) logical variable used by the SurveyMonkey
#' Excel exporter. It is used to evaluate if a multiple choice question was
#' answered or not.
#' @param column A column from a survey data frame where gender is recorded.
#' @param na_value  What should be the value of missing answers (Default: \code{"NA"}, but in some cases
#' it is useful to code them 0. )
#' @examples
#' is_question_viewed (column = c("Viewed", NA, "Any characters",
#'                         NA, NA),
#'                        na_value = 0 )
#' @export

is_question_viewed <- function ( column,
                          na_value = 0) {

   if ( is.null(na_value)) {
      warning("na_value is not given. Default NA missing sign is given.")
      na_value <- NA
    }

  column <- ifelse ( nchar(column) >0, 1, na_value)
  if(!is.na(na_value)) {
    column <- ifelse ( is.na(column), na_value, column )
  }

  if ( na_value %in% c(0,NA)) {
    return(as.numeric(column))
  }  else return(as.factor(column))

}
