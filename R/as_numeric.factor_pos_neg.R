#' Convert to numeric from a factor_pos_neg object
#'
#' @param x An object to be converted to numeric
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_pos_neg( c("Very good", "Rather Good",
#'                   "Inap.", "Rather Bad", "DK", "Very bad"))
#'
#' as_numeric (my_categories)
#'
#' @export
#'

as_numeric.factor_yes_no_4 <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_pos_neg (x)) {
    return(x[[3]])
  } else {
    stop("Not a factor_pos_neg object.")
  }
}

