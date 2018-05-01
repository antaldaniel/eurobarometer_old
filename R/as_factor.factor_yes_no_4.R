#' Convert to factor from a factor_yes_no_4 object
#'
#' @param x An object to be converted to factor
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_yes_no_4( c("Very good", "Rather Good",
#'                   "Inap.", "Rather Bad", "DK", "Very bad"))
#'
#' as_factor (my_categories)
#'
#' @export
#'

as_factor.factor_yes_no_4 <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_yes_no_4 (x)) {
    return(x[[2]])
  } else {
    stop("Not a factor_yes_no_4 object.")
  }
}

