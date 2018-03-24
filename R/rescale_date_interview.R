#' Rescale date of interview
#'
#' The function removes English day names, and English ordinal numbers such
#' as \code{1st}, \code{2nd}, \code{3rd}, \code{24th} are changed to cardinal
#' numbers \code{1}, \code{2}, \code{3}, \code{24}, etc.
#' @param column A column from a survey data where the date of interview is recorded.
#' @param return_class Default is \code{"Date"}, alternative is \code{"character"}.
#' @importFrom lubridate as_datetime
#' @importFrom stringr str_replace_all str_trim
#' @examples
#' rescale_date_interview (column =
#'                           c("Saturday 26th April 2014",
#'                             "Monday 28th April 2014"),
#'                         return_class = "character")
#' @export

rescale_date_interview <- function ( column,
                                 return_class = "Date") {

  if (!return_class %in% c("Date", "character")) {
    stop("Only Date and character return classes are allowed.")
  }

  if ("labelled" %in% class(column) ) {
    column <- haven::as_factor (column) }
  column <- as.character(column)

  remove_days =c("Monday" ="", "Tuesday" ="", "Wednesday" ="",
      "Thursday" ="", "Friday" ="", "Saturday" ="", "Sunday" ="")
  remove_chars =c("st" = "", "nd" ="", "rd" = "", "th" = "")
  column <- stringr::str_replace_all(column, remove_days)
  column <- stringr::str_replace_all(column, remove_chars)
  column <- gsub(",", "", column)
  column <- stringr::str_trim(column, side = "both")
  column <- lubridate::as_datetime(x = column, format = "%d %B %Y")
  column <- lubridate::as_date (column)
  if (return_class == "character") return(as.character(column))
  if (return_class == "Date") return(column)

}
