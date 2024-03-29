#' Convert to numeric from a factor_4 object
#'
#' @param x An object to be converted to numeric
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_4( c("Not at all", "Low",
#'                "Medium", "Strong", "DK"))
#'
#' as_numeric (my_categories)
#'
#' @export
#'

as_numeric.factor_4 <- function(x, type = NULL) {
  if(!is.null(type)) type <- NULL
  if (is.factor_4 (x)) {
    return(x[[3]])
  } else {
    stop("Not a factor_4 object.")
  }
}

