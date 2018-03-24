#' Suggest variable conversion
#'
#' @param x A variable from a GESIS archive.
#' @importFrom haven as_factor
#' @examples
#' \dontrun{
#' suggest_variable_conversion (x)
#' }
#' @export

class_conversion_suggest <- function (x) {

  if (class(x) %in% c("numeric", "integer")) return("numeric")
  if (class(x) %in% c("character")) return("character")
  if (class(x) %in% c("logical")) return("logical")
  if (class(x) %in% c("labelled")) {

    answer_options <- get_answer_options()

    x <- as.character(haven::as_factor(x))
    unique_values <- unique (x)[ ! unique(x) %in% c(NA, "DK", "Refusal")]
    unique_values <- unique_values[ ! grepl("inap", tolower(unique(x)))]

    test_values <- vapply ( 1:length(answer_options),
                            function (x) sum(
      ifelse ( unique_values %in%
                 answer_options[[x]], 1, 0)),
      numeric(1))

    if (sum( test_values > 0)) {
      return ( names(answer_options)[which.max(test_values)])
    } else {
      return ( "factor")
    }
  }

  else return("not numeric")
}
