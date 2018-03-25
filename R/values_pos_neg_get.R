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
                       "Better")
  factor_null     <- c("Enough", "Neither good nor bad",
                       "Neither",
                       "Same", "The same")
  factor_negative <- c("Too much", "A bad thing",
                       "Wrong direction",
                       "Worse")

  factor_pos_neg_list <- list ( factor_positive,
                                factor_null,
                                factor_negative)
  return(factor_pos_neg_list)
}
