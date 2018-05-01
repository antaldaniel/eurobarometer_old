#' Convert to character
#'
#' @param x An object to be converted
#' @param type If necessary, the type of conversion
#' @export

as_character <- function(x, type = NULL) UseMethod("as_character", x )
