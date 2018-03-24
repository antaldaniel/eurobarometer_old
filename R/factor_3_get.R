#' Get three positive level categories
#'
#' Get the character strings that will be represented by \code{0},
#' \code{1} or \code{2}.
#' @examples
#' factor_3_get()
#'
#' @export
#'

factor_3_get <- function () {
  factor_3_0 <- c("never", "Low",
                  "Rural area or village",
                  "The working class of society",
                  "Never acceptable",
                  "Almost never/never")
  factor_3_1 <- c("occasionally", "Medium",
                  "Small or medium-sized town",
                  "The middle class of society",
                  "Somtimes acceptable",
                  "From time to time")
  factor_3_2 <- c("frequently", "Strong",
                  "Large town/city",
                  "The upper class of society",
                  "Always acceptable",
                  "Most of the time")
  factor_3_list <- list ( factor_3_0, factor_3_1, factor_3_2)
  return(factor_3_list)
}
