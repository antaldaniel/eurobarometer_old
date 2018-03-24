#' Rescale the Eurobarometer occupation variable
#'
#' The Eurobarometer occupations have three groups: \code{none-active}, \code{self-employed},
#' and \code{employed} occupations. If you only need this categorization, you should
#' use the recatogorized version of this variable.
#' The numeric recategorization keeps this grouping, giving numeric representations
#' starting with digit \code{1} for \code{none-active}, \code{2} for \code{self-employed}
#' and \code{3} for \code{employed} respondents. Keeping only the first digit
#' will re-create the recoded version of this variable. The second digit is a unique
#' numeric identifier, but the ordering of the numeric variable, or the differences between
#' numeric codes have little if any meaning. This is a truly categorical variable.
#' The \code{return_class="factor"} and \code{return_class="character"} give shorthened
#' versions of the GESIS labels for easier handling in R.
#' This is a wrapper function around the \code{\link{rescale_categories}} function.
#' @param column A column from a survey data with the subjective urbanization answers.
#' @param from Defaults to \code{c("NULL")} in which case the GESIS coding in English
#' \code{c(..., "Retired, unable to work","Farmer", ...)} will
#' be used.
#' @param to Defaults to \code{NULL} in which case the GESIS coding will be
#' shortened to \code{c(... "retired", "farmer" ...)}. If you choose \code{return_class = "numeric"}
#' the default \code{to} parameter changes to \code{c(0,1,2)}.
#' @param na_labels  Defaults to \code{c("default")}.
#' @param underscore Defaults to \code{TRUE} in which case factor names or character strings
#' contain underscore_between_words.  This is a better approach for further programming,
#' but you can choose \code{FALSE} for nicer printing results. See examples.
#' @param return_class Default is \code{"factor"}, alternatives \code{"character"} or
#' \code{"numeric"}.
#' @examples
#' rescale_occupation (c("Owner of a shop, craftsmen, etc.",
#'                       "Retired, unable to work",
#'                       "Employed position, at desk",
#'                       "Employed professional (employed doctor, etc.)",
#'                       "DK"),
#'                     return_class = "character")
#'
#' ##Nicer printing factor names without underscore:
#'
#' rescale_occupation (c("Owner of a shop, craftsmen, etc.",
#'                       "Retired, unable to work","DK"),
#'                     underscore = FALSE)
#'
#' ##Beware that the numeric representation is grouped but not ordered.
#'
#' rescale_occupation (c("Owner of a shop, craftsmen, etc.",
#'                       "Retired, unable to work",
#'                       "Employed position, at desk",
#'                       "Employed professional (employed doctor, etc.)",
#'                       "DK"),
#'                     return_class = "numeric")
#'
#' @export

rescale_occupation <- function ( column,
                          from = NULL,
                          to = NULL,
                          na_labels = c("default"),
                          return_class = "factor",
                          underscore = TRUE) {
  if ( "labelled" %in% class(column)) {
    column <- haven::as_factor (column)
    column <- as.character(column)}

  if (is.null(from)) {
         from = c("Responsible for ordinary shopping, etc.",
                  "Student",
                  "Unemployed, temporarily not working",
                  "Retired, unable to work",
                  "Farmer",
                  "Fisherman",
                  "Professional (lawyer, etc.)",
                  "Owner of a shop, craftsmen, etc.",
                  "Business proprietors, etc.",
                  "Employed professional (employed doctor, etc.)",
                  "General management, etc.",
                  "Middle management, etc.",
                  "Employed position, at desk",
                  "Employed position, travelling",
                  "Employed position, service job",
                  "Supervisor",
                  "Skilled manual worker",
                  "Unskilled manual worker, etc.")
  }

  if(is.null(to)) {

    if ( return_class == "numeric") {
      to = c(11,12,13,14,
             21,22,23,24,25,
             31,32,33,34,35,36,37,38,39)

    } else {
      to =      c("household",
                  "student",
                  "not_working",
                  "retired",
                  "farmer",
                  "fisherman",
                  "professional",
                  "shop-owner",
                  "business-proprietor",
                  "employed_professional",
                  "genera_management",
                  "middle_management",
                  "employed_office",
                  "employed_travelling",
                  "employed_service",
                  "supervisor",
                  "skilled_manual",
                  "unskilled_manual")
    }

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

