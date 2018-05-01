#' Convert to factor from a factor_frequency object
#'
#' @param x An object to be converted to factor
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_frequency(
#'  x  = c("Less often", "Several times a month",
#'  "Every 2-3 months", "Once a month", "DK"))
#'
#' as_factor (my_categories)
#'
#' @export
#'

as_factor.factor_frequency <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_frequency (x)) {
    return(x[[2]])
  } else {
    stop("Not a factor_frequency object.")
  }
}

