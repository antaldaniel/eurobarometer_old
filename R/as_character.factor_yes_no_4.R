#' Convert to character from a factor_yes_no_4 object
#'
#' @param x An object to be converted to character
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_yes_no_4( c("Very good", "Rather Good",
#'                   "Inap.", "Rather Bad", "DK", "Very bad"))
#'
#' as_character (my_categories)
#'
#' @export
#'

as_character.factor_yes_no_4 <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_yes_no_4 (x)) {
    return(x[[1]])
  } else {
    stop("Not a factor_yes_no_4 object.")
  }
}

