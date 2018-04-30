#' Is the object of class factor_frequency?
#'
#' @param x An object to be checked if it is a quasi-numeric frequency
#' categorical variable
#' @examples
#' my_categories <- as_factor_frequency(
#'  x  = c("Less often", "Several times a month",
#'  "Every 2-3 months", "Once a month", "DK"))
#'
#' is.factor_frequency  (my_categories)
#'
#' @export
#'
is.factor_frequency <- function(x) inherits(x, "factor_frequency")
