#' Is the object of class factor_binary?
#'
#' @param x An object to be checked if it is a binary factor
#' @examples
#' my_binary <- as_factor_binary(c("mentioned", "mentioned",
#'                                 "not mentioned", "dk"))
#'
#' is.factor_binary ( my_binary)
#'
#' @export
#'
is.factor_binary <- function(x) inherits(x, "factor_binary")
