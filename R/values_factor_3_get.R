#' Get three level (positive, neutral, negative) categories
#'
#' @examples
#' values_factor_3_get()
#'
#' @export
#'

values_factor_3_get <- function () {
  factor_3_0 <- c("Low", "No", "never",
                  "Rural area or village",
                  "The working class of society",
                  "Almost never/never",
                  "Never acceptable")
  factor_3_1 <- c("Medium",
                  "Yes, once or twice",
                  "occasionally",
                  "Small or medium-sized town",
                  "The middle class of society",
                  "From time to time",
                  "Sometimes acceptable"
                  )
  factor_3_2 <- c("Strong",
                  "Yes, on several occasions",
                  "frequently",
                  "Large town/city",
                  "The upper class of society",
                  "Most of the time",
                  "Always acceptable")

  factor_3_list <- list ( factor_3_0, factor_3_1, factor_3_2)
  return(factor_3_list)
}
