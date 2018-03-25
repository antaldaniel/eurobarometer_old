#' Get four-level yes values
#'
#' Get the character strings that will be recoded as "yes" in
#' four-level variables.
#' @examples
#' values_yes_no_4_get()
#'
#' @export
#'

values_yes_no_4_get <- function () {
  yes_2_values <- c("Yes, definitely", "Very good",
                    "Very satisfied",
                    "Very optimistic", "Very positive",
                    "Strongly in favour", "Very effective",
                    "Totally agree")
  yes_1_values <- c("Yes, to some extent", "Rather good",
                    "Fairly satisfied",
                     "Fairly optimistic",
                    "Fairly positive", "Fairly in favour",
                    "Fairly effective",
                    "Tend to agree")
  no_1_values <- c("No, not really", "Rather bad",
                   "Not very satisfied",
                    "Fairly pessimistic",
                   "Fairly negative", "Fairly opposed",
                   "Not very effective",
                   "Tend to disagree")
  no_2_values <- c("No, definitely not", "Very bad",
                   "Not at all satisfied",
                    "Very pessimistic", "Very negative",
                   "Strongly opposed", "Not at all effective",
                   "Totally disagree")
  yes_4_list <- list ( yes_2_values, yes_1_values,
                       no_1_values,   no_2_values)
  return(yes_4_list)
}
