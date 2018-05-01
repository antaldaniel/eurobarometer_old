#' Convert to character from a factor_frequency object
#'
#' @param x An object to be converted to character
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_frequency(
#'  x  = c("Less often", "Several times a month",
#'  "Every 2-3 months", "Once a month", "DK"))
#'
#' as_character (my_categories)
#'
#' @export
#'

as_character.factor_frequency <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_frequency (x)) {
    return(x[[1]])
  } else {
    stop("Not a factor_frequency object.")
  }
}

