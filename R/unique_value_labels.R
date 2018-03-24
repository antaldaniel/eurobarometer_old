#' Summarize unique value labels of a variable.
#'
#' @param x A variable to summarize.
#' @param numeric_values Defaults to \code{FALSE} in which case the integer
#' and numeric variables are no analyized. If \code{(TRUE)} it will create
#' a similar character summary of the numeric variables.
#' @examples
#' unique_value_labels (x = c("apple", "strawberry", NA, "apple"))
#'
#' #By default will not summarize numeric variables
#' unique_value_labels (x = c(1,2, NA, 3))
#'
#' #But it is possible to create a similar summary of numeric variables.
#' unique_value_labels (x = c(1,2, NA, 3),
#'                      numeric_values = TRUE)
#' @export


unique_value_labels <- function ( x, numeric_values = FALSE ) {

  if ( "data.frame" %in% class(x) )  { return (as.character("")) }
  if ( "list" %in% class(x) )        { return (as.character("")) }

  if ( "labelled" %in% class(x)) {
    x <- as.character(haven::as_factor(x))
  }
  if ( "factor" %in% class(x)) {
    x <- as.character(x)
  }
  if ( class(x)[1] %in%  c("numeric", "integer") ) {
    if ( numeric_values == FALSE) return("")
    x <- as.character(x)
  }

  unique_values <- unique(x)
  unique_values <- unique_values [!is.na(unique_values)]
  x <- substr ( paste ( unique_values,
                        collapse = "|"), 1, 300)

  return (x)

}
