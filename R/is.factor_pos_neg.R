#' Is the object of class factor_pos_neg?
#'
#' @param x An object to be checked if it is a 3-level (positive,
#' neutral, negative) categorical variable.
#' @examples
#' my_categories <- as_factor_pos_neg( c("Better", "DK", "Worse",
#'                                "Same", "The Same","Inap. not "))
#'
#' is.factor_pos_neg (my_categories)
#'
#' @export
#'
is.factor_pos_neg<- function(x) inherits(x, "factor_pos_neg")
