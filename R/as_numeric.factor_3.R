#' Convert to numeric from a factor_3 object
#'
#' @param x An object to be converted to numeric
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @examples
#' my_categories <- as_factor_3( c("Rural area or village",
#'  "Small or medium-sized town", "Inap.", "Large town/city",
#'   "DK", "Large town/city"))
#'
#' as_numeric (my_categories)
#'
#' @export
#'

as_numeric.factor_3 <- function(x, type = NULL) {

  if (is.factor_3 (x)) {
    return(x[[3]])
  } else {
    stop("Not a factor_binary object.")
  }
}

