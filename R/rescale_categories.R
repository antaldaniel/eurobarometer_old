#' Rescale categories
#'
#' A generic function to rescale categorical variables. \cr
#' There are wrapper functions available for the Eurobarometer trend
#' and demography questions with the default GESIS codes:\cr
#' For example: \cr
#' \code{\link{rescale_marital_status}} for marital status. \cr
#' \code{\link{rescale_occupation}} for occupation and last job. \cr
#' The \code{\link{get_answer_options}} functions lists all the
#' currently recognized standard values.
#' @param column A column from a survey data frame with categorical variables.
#' @param from Defaults to \code{c("Agree", "Disagree")}. Shorthened categories can be
#' used, to, in this case the first part of the character string is matched, for example,
#' \code{"Totally agree"} in case of \code{from = "totally"} is matched for the first
#' six characters,  \code{"not agree"} in case of \code{from = "not"}  is matched against
#' the first three characters.
#' @param exact_from Deafults to \code{TRUE}. If \code{FALSE} you can use the
#' partial matching, but beware that in this case, \code{"twenty"} will be replaced by
#' \code{20}, and \code{"twenty-one"} will be already matched with \code{"twenty"}.
#' @param to Defaults to \code{c(1,0)}.
#' @param na_labels  Defaults to \code{NULL}. The pre-coded profiles can be accessed via
#' \code{\link{get_na_labels}}.
#' @param return_class Default is \code{"numeric"}, alternatives \code{"character"} or
#' \code{"factor"}.
#' @importFrom haven as_factor
#' @importFrom stringr str_trim
#' @examples
#' rescale_categories (column =
#'         c("Agree", "AGREE", "disagree",
#'           "DK", "Refusal (SPONT.)", "Disagree"),
#'         na_labels = c("DK", "Refusal (SPONT.)")
#'         )
#'
#' rescale_categories (column =
#'         c("Agree", "AGREE", "disagree",
#'           "DK", "Refusal (SPONT.)", "Disagree"),
#'         na_labels = "default")
#'
#' rescale_categories (column = c("Agree", "AGREE", "disagree"))
#'
#' @export

rescale_categories <- function ( column,
                          from = c("Agree", "Disagree"),
                          to = c(1,0),
                          na_labels = NULL,
                          exact_from = TRUE,
                          return_class = "numeric") {
  if (is.null(column)) {
    stop ("No variable was given for rescaling.")
  }
  if ( ! return_class[1] %in% c("numeric", "factor", "character")) {
    stop("Only numeric, factor or character type returns are allowed")
  }
  if ( length(from) != length(to) ) {
    stop("From and to vectors must be of equal length")
  }

  if ( "labelled" %in% class(column)  ) {
    column <- haven::as_factor (column) }


  column <- tolower(as.character(column))
  column <- stringr::str_trim ( column, side = "both")

  column <- ifelse (grepl("inap", tolower(column)),
                    yes = NA, no = column)

  if(!is.null(na_labels)) {
    na_label_variations <- get_na_labels()

    if ( all(length(na_labels) == 1 & is.na(na_labels)) ) {
      na_change <- NA
    } else if (na_labels[1] == "default") {
        na_change = na_label_variations$default
    } else if (na_labels[1] == "hu") {
        na_change = na_label_variations$hu
    } else if (na_labels[1] == "gesis") {
      na_change = na_label_variations$gesis
    } else {
      message ( "Custom NA labelling" )
      na_change <- as.character(na_labels)
    } #end else

    na_change <- tolower(na_change)
    if (! is.na(na_change[1])) {
      for ( i in 1:length(na_change)) {
        match_length <- nchar(na_change[i])
        temp_column  <- stringr::str_sub(as.character(column),
                                         1, match_length)
        temp_column <- ifelse(temp_column == na_change[i],
                              NA, column )
        column      <- ifelse(is.na(temp_column),
                              yes  = NA, no = column)
    }


    } #end of na replacement cycle
    } #end of !null na_labelling.

  from_short <- tolower(as.character(from [which(!is.na(from))])) #not including NA values

  if ( exact_from == FALSE ) {
    for ( i in 1:length(from_short)) {
      match_length <- nchar(from_short[i])
      temp_column <- stringr::str_sub(as.character(column),
                                      1, match_length)
      column <- ifelse(temp_column == from_short[i],
                       yes  = to[i], no = column)
    }
  } else {
    for ( i in 1:length(from)) {
      column <- ifelse(column == tolower(as.character(from[i])),
                       yes  = to[i], no = column )
    } #end of loop
  } #end of else

  if ( return_class == "character" ) return(as.character(column))
  if ( return_class == "numeric" ) return(as.numeric(as.character(column)))
  if ( return_class == "factor" ) return(as.factor(as.character(column)))
}
