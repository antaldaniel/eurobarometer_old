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
                  "Not at all important",
                  "Not at all likely",
                  "Not at all interested",
                  "Not at all interested in politics",
                  "Not at all attached",
                  "Not at all satisfied",
                  "Not at all satisfactory",
                  "Protects not at all",
                  "It applies very well",
                  "Not recognised")
  factor_4_1 <- c("Low",
                  "Not very informed",
                  "Not very important",
                  "Not very likely",
                  "Not very interested",
                  "Slightly interested in politics",
                  "Not very attached",
                  "Not very satisfied",
                  "Not very satisfactory",
                  "Protects not very well",
                  "It applies fairly well",
                  "Recognised to some extent")
  factor_4_2 <- c("Medium",
                  "Fairly well informed",
                  "Fairly important",
                  "Fairly likely",
                  "Fairly interested",
                  "Moderately interested in politics",
                  "Fairly attached",
                  "Fairly satisfactory",
                  "Fairly satisfied",
                  "Protects fairly well",
                  "It does not apply very well",
                  "Largely recognised")
  factor_4_3 <- c("Strong",
                  "Very well informed",
                  "Very important",
                  "Very likely",
                  "Very interested",
                  "Strongly interested in politics",
                  "Very attached",
                  "Very satisfied",
                  "Very satisfactory",
                  "Protects very well",
                  "It does not apply at all",
                  "Completely recognised")

  factor_4_list <- list ( factor_4_0, factor_4_1,
                          factor_4_2, factor_4_3)
  return(factor_4_list)
}
