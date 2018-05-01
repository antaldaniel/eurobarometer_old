#' Convert to character from a factor_pos_neg object
#'
#' @param x An object to be converted to character
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_pos_neg(
#'    c("Better", "DK", "Worse",
#'      "Same", "The Same", "Inap. not")
#'      )
#'
#' as_character (my_categories)
#'
#' @export
#'

as_character.factor_pos_neg <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_pos_neg (x)) {
    return(x[[1]])
  } else {
    stop("Not a factor_pos_neg object.")
  }
}

