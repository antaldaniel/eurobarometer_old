#' Convert to numeric
#'
#' @param x An object to be converted
#' @param type If necessary, the type of conversion
#' @examples
#' my_binary <- as_factor_binary(c("mentioned", "mentioned",
#'  "not mentioned", "dk"))
#'
#' as_numeric (my_binary)
#'
#' @export

as_numeric <- function(x, type = NULL) UseMethod("as_numeric", x )
