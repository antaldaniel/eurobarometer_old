#' Convert to numeric from a factor_binary object
#'
#' @param x An object to be converted
#' @param type Default to \code{NULL}. Not used in this instance
#' of the method.
#' @importFrom haven is.labelled
#' @examples
#' my_binary <- as_factor_binary(c("mentioned", "mentioned",
#'  "not mentioned", "dk"))
#'
#' as_numeric (my_binary)
#'
#' @export
#'

as_numeric.factor_binary <- function(x, type = NULL) {

  if(!is.null(type)) type <- NULL
  if (is.factor_binary(x)) {
    return(x[[3]])
  } else if(haven::is.labelled(x) ) {
    x <- as.character(haven::as_factor(x))
    possible_values <- as_factor_binary (x)
    return (possible_values[[3]])
    } else if (is.character(x)) {
      possible_values <- as_factor_binary(x)
      return (possible_values[[3]])
      }else {
    stop("Not a factor_binary or labelled object.")
  }
}
