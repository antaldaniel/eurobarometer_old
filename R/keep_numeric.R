#' Keep numeric elements in a string
#'
#' @param column  column from a survey data frame where numbers are included with
#' units or other test, such as \code{"15 years old"}.
#' @param na_labels An explicit missing label to be removed such as "refusal".
#' The default value is \code{NULL}, but \code{"default"} will use a setting
#' \code{c("refusal", "nt/nv", "dk", "dk/na")} and remove all observations
#' starting with a label \code{Inap.}.
#' @param max_value Optional maximum value, anything reaching this level will be replaced with \code{NA}.
#' @param min_value Optional maximum value, anything reaching this level will be replaced with \code{NA}.
#' @param more_label Optional label for text "more" or similar.
#' @param less_label Optional label for text "less" or similar.
#' @param comma If decimals are written with commas, such as 10,7 instead of 10.7
#' Defaults to \code{FALSE}.
#' @importFrom haven as_factor
#' @importFrom stringr str_trim str_extract
#' @examples
#' keep_numeric("He is 183.5 cm tall")
#'
#' keep_numeric("He is 183,5 cm tall", comma=TRUE)
#'
#' keep_numeric("He is 383.5 cm tall", max_value = 250)
#' @export
#'

keep_numeric <- function ( column,
                           na_labels = NULL,
                           max_value = NULL,
                           min_value = NULL,
                           more_label = NULL,
                           less_label = NULL,
                           comma = FALSE) {
  ##temporary
  more_value <- 0
  less_value <- 0
  if(!is.null(max_value)) {
    if ( class(max_value) != "numeric" ) {
      stop("The max_value must be a number.")
    }
  }

  if(!is.null(min_value)) {
    if (class(min_value) != "numeric" ) {
    stop("The min_value must be a number.")
    }
  }

  if(!is.null(less_label)) {
    if ( class(less_label) != "numeric" ) {
      stop("The max_value must be a number.")
    }
  }

  if(!is.null(more_value)) {
    if (class(more_value) != "numeric" ) {
      stop("The more_value must be a number.")
    }
  }

  if ( "labelled" %in% class(column) ) {
    column <- haven::as_factor (column)
  }

  column <- tolower(as.character(column))
  column <- stringr::str_trim(column, side = "both")

  if(!is.null(na_labels)) {
    if ( na_labels[1] == "default") {
      na_labels = c("refusal", "nt/nv", "dk", "dk/na")
      column <- ifelse(grepl("Inap.", column), NA, column)
    }
    column <- ifelse(column %in% na_labels, NA, column)
  }

  if(!is.null(more_label)) {
    more_labels <- c("or more", "and more", "vagy t\u00F6bb",
                     "enn\u00E9l t\u00F6bb")
    column <- ifelse(column %in% na_labels, more_value, column)
  }

  if ( comma == TRUE )   {
    column <- gsub(",", "\\.", column) }
  column <- paste0("rem ", column) ##does not start from beginning

  column <- stringr::str_extract(column,
                        "[+-]?( (\\d+(\\.\\d*)?)|(\\.\\d+) )")
  column <- stringr::str_trim(column, side = "both")
  column <- as.numeric(as.character(column))
  if (! is.null(max_value)) {
    column <- ifelse(column >= max_value, NA, column)
  }
  if (! is.null(min_value)) {
    column <- ifelse(column <= min_value, NA, column)
  }
  return(column)
}
