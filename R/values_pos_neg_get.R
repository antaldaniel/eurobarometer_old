#' Get three level (positive, neutral, negative) categories
#'
#' @examples
#' values_pos_neg_get()
#'
#' @export
#'

values_pos_neg_get <- function () {
  factor_positive <- c("Too little", "A good thing",
                       "Right direction",
                       "Better", "Too little",
                       "Too modest", "A positive impact",
                       "Improved",
                       "Very strongly",
                       "Positive effect",
                       "Higher")
  factor_null     <- c("Enough", "Neither good nor bad",
                       "Neither",
                       "Same", "The same",
                       "About the right amount",
                       "About right", "No impact",
                       "Stayed about the same",
                       "No effect",
                       "Equal")
  factor_negative <- c("Too much", "A bad thing",
                       "Wrong direction",
                       "Worse", "Too much",
                       "Too ambitious",
                       "A negative impact",
                       "Got worse",
                       "Not at all strongly",
                       "Negative effect",
                       "Lower")

  factor_pos_neg_list <- list ( factor_positive,
                                factor_null,
                                factor_negative)
  return(factor_pos_neg_list)
}
