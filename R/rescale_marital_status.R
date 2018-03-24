#' Rescale the Eurobarometer marital status variable.
#'
#' If you only need a simpler categorization, use the 4- or 5- category
#' versions of this variable.
#' This is a truly categorical variable. The \code{return_class="factor"}
#' and \code{return_class="character"} give shorthened versions of the GESIS labels for easier handling in R.
#' This is a wrapper function around the \code{\link{rescale_categories}} function.
#' @param column A column from a survey data with the subjective urbanization answers.
#' @param from Defaults to \code{c("NULL")} in which case the GESIS coding in English
#' \code{c("Widow/er: living without children","Single living w partner: without children" ...} will
#' be used.
#' The GESIS coding is not same across files.
#' @param to Defaults to \code{NULL} in which case the GESIS coding will be
#' shortened to \code{c("widow_no_children", "partners_no_children" ...)}.
#' @param na_labels  Defaults to \code{c("default")}.
#' @param underscore Defaults to \code{TRUE} in which case factor names or character strings
#' contain underscore_between_words.  This is a better approach for further programming,
#' but you can choose \code{FALSE} for nicer printing results. See examples.
#' @param return_class Default is \code{"factor"} the alternative is \code{"character"}.
#' @examples
#' rescale_marital_status (c("Widow/er: living without children",
#'                           "(Re-)Married: w children of this marriage",
#'                           "DK"),
#'                         return_class = "character",
#'                         underscore = FALSE)
#' @export

rescale_marital_status <- function ( column,
                          from = NULL,
                          to = NULL,
                          na_labels = c("default"),
                          return_class = "factor",
                          underscore = TRUE) {

  if (! return_class %in% c("factor", "character")) {
    stop('Only return class = "factor" or "character" is supported.')
  }
  if ( "labelled" %in% class(column)) {
    column <- haven::as_factor (column)
    column <- as.character(column)
    }

  if (is.null(from)) {
    from  = c(
      "Widow/er: living without children",
      "Widow: without children",
      "Single living w partner: without children",
      "Single liv w partner: without children",
      "Single living w partner: w children of this union",
      "Single liv w partner: childr this union",
      "Single liv w partner: childr prev union" ,
      "(Re-)Married: w children of this marriage",
      "(Re-)Married: children this marriage",
      "(Re-)Married: without children",
      "(Re-)Married: living without children",
      "(Re-)Married: children this/prev marriage",
      "Single: living without children",
      "Single: without children",
      "Single: with children",
      "Single: living with children",
      'Widow/er: living with children',
      "Widow: with children",
      "Single living w partner: w children of this and previous union",
      "Divorced/Separated: living without children",
      "Divorced/Separated: without children",
      "Divorced/Separated: with children",
      "Divorced/Separated: living with children",
      "Other (SPONT.)",
      "Refusal (SPONT.)")

  }

  if(is.null(to)) {
      to =  c(
        "widow_no_children","widow_no_children",
        "partners_no_children","partners_no_children",
        "partners_own_children", "partners_own_children",
        "partners_with_children",
        "married_own_children","married_own_children",
        "married_no_children","married_no_children",
        "married_with_children",
        "single_no_children","single_no_children",
        "single_with_children", "single_with_children",
        "widow_with_children", "widow_with_children",
        "partners_no_children",
        "separated_no_children","separated_no_children",
        "separated_with_children", "separated_with_children",
        "other",
        NA
      )
    }

  if ( underscore == FALSE ) {
    to <- gsub("_", " ", to)
  }

  return(rescale_categories(column = column,
                            from = from, to = to,
                            na_labels = na_labels,
                            exact_from = TRUE,
                            return_class = return_class))

}

