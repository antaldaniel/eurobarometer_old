#' Get four level non-negative categories
#'
#' @examples
#' values_factor_4_get()
#'
#' @export
#'

values_factor_4_get <- function () {
  factor_4_0 <- c("Not at all",
                  "Not at all informed",
                  "Not at all likely",
                  "Not at all interested",
                  "Not at all interested in politics",
                  "Not at all interested in politics")
  factor_4_1 <- c("Low",
                  "Not very informed",
                  "Not very likely",
                  "Not very interested",
                  "Slightly interested in politics"
                  )
  factor_4_2 <- c("Medium",
                  "Fairly well informed", "Fairly likely",
                  "Fairly interested",
                  "Moderately interested in politics")
  factor_4_3 <- c("Strong",
                  "Very well informed",
                  "Very likely",
                  "Very interested",
                  "Strongly interested in politics")

  factor_4_list <- list ( factor_4_0, factor_4_1,
                          factor_4_2, factor_4_3)
  return(factor_4_list)
}
