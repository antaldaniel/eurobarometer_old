#' Get standard NA labels.
#'
#' @examples
#' get_na_labels()
#'
#' ##Create a custom NA label filter
#' my_na_filter <- c(get_na_labels()$default,
#'                   "Not_among_these")
#' cat(my_na_filter)
#' @export

get_na_labels <- function () {
  answer_options <- list (
    default = c("DK", "NA", "Missing", "Refusal", "Inap",
                "NT/NV", "N/A"),
    hu = c("NA - nem tudja", "NT/NV", "NA", "DK", "N/A",
           "nem v", "NA - nem tud"),
    gesis = c("DK", "Inap.", "Refusal"),
    stringsAsFactors = FALSE
  )
  return (answer_options)
}
