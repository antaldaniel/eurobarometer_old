#' Rescale alphanumeric variables in English
#'
#' Create a numeric (or other) variable from alphanumeric coding, such as
#' "twenty", "twenty-one".\cr
#' This is a wrapper function around the\code{\link{rescale_categories}} function.
#' @param column A column from a survey data with a four-level agreement scale.
#' @param from Defaults to \code{c("NULL")} in which case the numbers 0-40
#' and 45 are coded, for example, from \code{"Twenty-one"}.
#' @param to Defaults to \code{c(0:40,41,NA)}.
#' @param na_labels  Defaults to \code{c("default")}.
#' @param return_class Default is \code{"numeric"}, alternatives \code{"character"} or
#' \code{"factor"}.
#' @examples
#' rescale_alphanumeric_en  (column = c("One", "Twenty",
#'                            "twenty-one", "sEven", "NA"),
#'                           na_labels = "default")
#' @export

rescale_alphanumeric_en <- function ( column,
                          from = NULL,
                          to = NULL,
                          na_labels = c("default"),
                          return_class = "numeric") {

  if (is.null(from)) {
    from = c("Null", "Zero", "One", "Two", "Three", "Four", "Five", "Six",
             "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve",
             "Thirteen", "Fourteen", "Fifteen", "Sixteen",
             "Seventeen", "Eighteen", "Nineteen", "Twenty",
             "Twenty-one", "Twenty-two", "Twenty-three",
             "Twenty-four", "Twenty-five", "Twenty-six",
             "Twenty-seven", "Twenty-eight", "Twenty-nine",
             "Thirty", "Thirty-one", "Thirty-two", "Thirty-three",
             "Thirty-four", "Thirty-five", "Thirty-six", "Thirty-seven",
             "Thirty-eight", "Thirty-nine", "Fourty",
             "Fourty-five",
             "DK/NA (cases from RO HR); NOT CODED (cases from IE FI SI)")
  }

  if(is.null(to)) {
    to = c(0,0,1,2,3,4,5,6,7,8,9,10,
           11,12,13,14,15,16,17,18,19,20,
           21,22,23,24,25,26,27,28,29,30,
           31,32,33,34,35,36,37,38,39, 40,
           45,NA)
  }

  return(rescale_categories(column, from, to,
                            na_labels, exact_from = TRUE,
                            return_class))

}




