#' Get binary yes values
#'
#' Get the character strings that will be recoded as "yes" in
#' binary variables.
#' @examples
#' binary_values_get()
#'
#' @export
#'

binary_values_get <- function () {
  yes_values <- c("igen", "yes", "mentioned",
                  "da", "agree", "for",
                  "tend to trust",
                  "woud benefit",
                  "has benefited",
                  "female", "Woman",
                  "Doing a good job",
                  "Correct answer",
                  "true",
                  "True (correct)",
                  "correct")
  no_values <- c("nem", "no", "not mentioned",
                 "net", "disagree", "against",
                 "tend not to trust",
                 "would not benefit",
                 "has not benefited", "male", "Man",
                 "Not doing a good job",
                 "Wrong answer",
                 "false",
                 "false (incorrect)",
                 "incorrect")
  binary_list <- list ( yes_values,  no_values)
  return(binary_list)
}
