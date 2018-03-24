#' Rescale time frequency of use
#'
#' Create a uniform variable from use frequency categories such as code{
#' c("Everyday/Almost everyday", "Two or three times a week",
#' "About once a week", "Two or three times a month",
#' "Less often", "Never")}. This is a wrapper function around the
#' \code{\link{rescale_categories}} function.
#' @param column A column from a survey data frame where frequency should be
#' converted to a numeric value.
#' @param from Defaults to \code{c("Everyday/Almost everyday",
#' "Two or three times a week","About once a week",
#' "Two or three times a month", "Less often", "Never")}.
#' @param to Defaults to \code{c(250,120, 50,25,12,0)}.
#' @param na_labels  Defaults to \code{c("NA","DK")}.
#' @param exact_from Deafults to \code{TRUE}. If \code{FALSE} you can use the
#' partial matching, but beware that in this case, \code{"twenty"} will be replaced by
#' @param return_class Default is \code{"numeric"}, alternatives \code{"character"} or
#' \code{"factor"}.
#' @importFrom haven as_factor
#' @importFrom plyr mapvalues
#' @importFrom stringr str_trim
#' @examples
#' rescale_time_frequency_use (column = c(
#'  "Everyday/Almost everyday", "Less often", "never", "NEVER", "NA", "DK"),
#'  na_labels = "default")
#' @export

rescale_time_frequency_use <- function ( column,
                          from = c("Everyday/Almost everyday", "Two or three times a week",
                                   "About once a week", "Two or three times a month",
                                   "Less often", "Never"),
                          to = c(250,120, 50,25,12,0),
                          exact_from = TRUE,
                          na_labels = c("NA","DK"),
                          return_class = "numeric") {
  if ( "labelled" %in% class(column)) {
    column <- haven::as_factor (column) }

  column <- tolower(as.character(column))
  column <- stringr::str_trim ( column, side = "both")
  column <- ifelse(grepl("no internet access", column),
                    yes = "never", no = column)
  column <- ifelse(grepl("no access", column),
                   yes = "never", no = column)
  column <- ifelse(grepl("inap.", column),
                   yes = NA, no = column)
  return(rescale_categories(column = column,
                            from = from, to = to,
                            na_labels = na_labels,
                            exact_from = exact_from,
                            return_class = return_class))

}
