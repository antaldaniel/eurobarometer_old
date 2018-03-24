#' Get three level positive, zero, negative factors
#'
#' Get the character strings that will be represented by \code{-1},
#' \code{0} or \code{1}.
#' @examples
#' factor_pos_neg_get()
#'
#' @export
#'

factor_pos_neg_get <- function () {
  factor_positive <- c("Too little", "A good thing",
                       "Right direction",
                       "Better")
  factor_null     <- c("Enough", "Neither good nor bad",
                       "Neither",
                       "Same")
  factor_negative <- c("Too much", "A bad thing",
                       "Wrong direction",
                       "Worse")

  factor_pos_neg_list <- list ( factor_positive,
                                factor_null,
                                factor_negative)
  return(factor_pos_neg_list)
}
