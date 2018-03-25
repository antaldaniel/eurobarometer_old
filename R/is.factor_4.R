#' Is the object of class factor_4?
#'
#' @param x An object to be checked if it is a 4-level
#' non-negative categorical variable
#' @examples
#' my_categories <- as_factor_4( c("Not at all", "Low",
#'                "Medium", "Strong", "DK"))
#'
#' is.factor_4  (my_categories)
#'
#' @export
#'
is.factor_4 <- function(x) inherits(x, "factor_4")
