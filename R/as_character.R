#' Convert to character
#'
#' @param x An object to be converted
#' @param type If necessary, the type of conversion
#' @examples
#' my_binary <- as_factor_binary(c("mentioned", "mentioned",
#'  "not mentioned", "dk"))
#'
#' as_character (my_binary)
#'
#' @export

as_character <- function(x, type = NULL) UseMethod("as_character", x )
