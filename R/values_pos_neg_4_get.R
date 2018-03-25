#' Get four level (2 positive, 2 negative) categories
#'
#' @examples
#' values_pos_neg_get()
#'
#' @export
#'

values_pos_neg_4_get <- function () {
  factor_positive <- c("Too little", "A good thing",
                       "Right direction",
                       "Better", "Too little",
                       "Too modest")
  factor_null     <- c("Enough", "Neither good nor bad",
                       "Neither",
                       "Same", "The same",
                       "About the right amount",
                       "About right")
  factor_negative <- c("Too much", "A bad thing",
                       "Wrong direction",
                       "Worse", "Too much",
                       "Too ambitious")

  factor_pos_neg_list <- list ( factor_positive,
                                factor_null,
                                factor_negative)
  return(factor_pos_neg_list)
}
