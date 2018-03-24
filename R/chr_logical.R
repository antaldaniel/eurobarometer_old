#' Character to logical conversion
#'
#' Converts logical answers from text documents.
#' @param column A column from a survey data frame where gender is recorded.
#' @param false_id The false values in the original raw data. defaults to \code{"0"}.
#' \code{"default"} searches for statements starting with \code{c("nincs", "none", "ne", "no", "non", "never")}.
#' @param true_id The true values in the original raw data, defaults to \code{"1"}.
#' \code{"default"} searches for statements starting with \code{c("van", "igen", "yes", "da", "oui")}.
#' @param na_labels  Is there a special character ID for missing variables? (Default: \code{"default"} )
#' @param na_value  What should be the value of missing answers (Default: \code{"NA"}, but in some cases
#' it is useful to code them 0. )
#' If you get a warning with NA conversion, you are likely to have several NA characters,
#' such as \code{missing} and \code{NA} at the same time.
#' @examples
#'chr_logical(column = as.character (c("1 - yes", "1", "0 - no",
#'            "missing", NA)),
#'            false_id  = "0 - no",
#'            true_id = "1 - yes",
#'            na_labels = "missing" )
#' @export

chr_logical <- function ( column,
                          false_id = "default",
                          true_id  = "default",
                          na_labels    = "default",
                          na_value = NA) {

  if ( is.null(false_id)) stop("False identifier is not given.")
  if ( is.null(true_id))  stop("True identifier is not given.")
  if ( is.null(na_labels)) {
    warning("na_labels is not given. Default options are used.")
    na_labels <- "default"
  }
  if ( is.null(na_value)) {
      warning("na_value is not given. Default NA missing sign is given.")
      na_value <- NA
    }

  column <- tolower(as.character ( column ))
  if (! is.null(na_labels) ) {
    if ( na_labels[1] == "default") {
      na_labels <- c("na", "dk", "inap", "refusal", "nt/nv", "nem tudja")
    }

    na_labels <- tolower(as.character(na_labels))

    for (i in 1:length(na_labels)) {
      n_na    <- nchar ( na_labels[i] )
      column <- ifelse ( substr(column, 1, n_na) == na_labels[i],
                         yes = na_value, no = column ) #explict na replacement

    }
  }

  if ( false_id == "default" ) {
    potential_false <- c("nincs", "none", "ne", "no", "non", "never")
  } else {
      potential_false  <- false_id
  }
  for ( i in 1:length(potential_false)) {
    n_false <- nchar ( potential_false[i] )
    column <- ifelse ( substr(column, 1, n_false) == potential_false[i],
                       yes = 0, no = column )
  }

  if ( true_id == "default" ) {
    potential_true <- c("van", "igen", "yes", "da", "oui") } else {
      potential_true = true_id
    }

  for ( i in 1:length(potential_true)) {
    n_false <- nchar ( potential_true[i] )
    column <- ifelse ( substr(column, 1, n_false) == potential_true[i],
                       yes = 1, no = column )
  }


  column <- ifelse ( column %in% c(NA, 1, 0), column,
                     -1 )

  if (-1 %in% unique (column)) warning (
    "There were invalid values that were coded as -1.
     Please check the original data file, or recode negative values."
  )

  return(as.numeric(column))

}
