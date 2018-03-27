#' Get three level (positive, neutral, negative) categories
#'
#' @examples
#' values_pos_neg_get()
#'
#' @export
#'

values_pos_neg_get <- function () {
  factor_positive <- c("Too little", "Not doing enough",
                       "A good thing",
                       "Right direction",
                       "Things are going in the right direction",
                       "Better", "Too little",
                       "Too modest", "A positive impact",
                       "Improved",
                       "Very strongly",
                       "Positive effect",
                       "Higher", "Insufficient",
                       "Too negatively",
                       "More important")
  factor_null     <- c("Enough", "Doing about the right amount",
                       "Neither good nor bad",
                       "Neither",
                       "Neither the one nor the other (SPONTANEOUS)",
                       "Neither the one nor the other",
                       "Same", "The same",
                       "About the right amount",
                       "About right", "No impact",
                       "Stayed about the same",
                       "No effect",
                       "Equal", "Adequate/ about right",
                       "Objectively",
                       "No change / As it is now (SPONTANEOUS)",
                       "No change / As it is now")
  factor_negative <- c("Too much", "Doing too much",
                       "A bad thing",
                       "Wrong direction",
                       "Things are going in the wrong direction",
                       "Worse", "Too much",
                       "Too ambitious",
                       "A negative impact",
                       "Got worse",
                       "Not at all strongly",
                       "Negative effect",
                       "Lower", "Excessive", "Too positively",
                       "Less important")

  factor_pos_neg_list <- list ( factor_positive,
                                factor_null,
                                factor_negative)
  return(factor_pos_neg_list)
}
