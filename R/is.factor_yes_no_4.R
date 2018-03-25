#' Is the object of class factor_yes_no_4?
#'
#' @param x An object to be checked if it is a 4-level (2 positive,
#' 2 negative) categorical variable.
#' @examples
#' my_categories <- as_factor_yes_no_4(
#'     c("Very good", "Rather Good",
#'     "Inap.", "Rather Bad", "DK", "Very bad")
#'     )
#'
#' is.factor_yes_no_4 (my_categories)
#'
#' @export
#'
is.factor_yes_no_4 <- function(x) inherits(x, "factor_yes_no_4")
