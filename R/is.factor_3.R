#' Is the object of class factor_3?
#'
#' @param x An object to be checked if it is a 3-level positive
#' categorical variable.
#' @examples
#' my_categories <- as_factor_3( c("Rural area or village",
#'  "Small or medium-sized town", "Inap.", "Large town/city",
#'   "DK", "Large town/city"))
#'
#' is.factor_3  (my_categories)
#'
#' @export
#'
is.factor_3 <- function(x) inherits(x, "factor_3")
