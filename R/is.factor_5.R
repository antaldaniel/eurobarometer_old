#' Is the object of class factor_5?
#'
#' @param x An object to be checked if it is a 5-level positive
#' categorical variable.
#' @examples
#' my_categories <- as_factor_5( c("Much lower", "Somewhat lower",
#' 	"More or less the same", 	"Somewhat higher", "Much higher")
#'
#' is.factor_5 (my_categories)
#'
#' @export
#'
is.factor_5 <- function(x) inherits(x, "factor_5")
