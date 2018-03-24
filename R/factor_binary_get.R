#' Get four-level yes values
#'
#' Get the character strings that will be recoded as "yes" in
#' four-level variables.
#' @examples
#' yes_4_get
#'
#' @export
#'

factor_binary_get <- function () {
  yes_values <- c("igen", "yes", "mentioned",
                  "da", "agree", "for",
                  "tend to trust",
                  "woud benefit",
                  "female")
  no_values <- c("nem", "no", "not mentioned",
                 "net", "disagree", "against",
                 "tend not to trust",
                 "would not benefit", "male")
  binary_list <- list ( yes_values,  no_values)
  return(binary_list)
}
