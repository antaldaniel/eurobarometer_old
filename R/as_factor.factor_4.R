#' Convert to factor from a factor_4 object
#'
#' @param x An object to be converted to factor
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_4( c("Not at all", "Low",
#'                "Medium", "Strong", "DK"))
#'
#' as_factor (my_categories)
#'
#' @export
#'

as_factor.factor_4 <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_4 (x)) {
    x[[2]]
  } else {
    stop("Not a factor_4 object.")
  }
}

