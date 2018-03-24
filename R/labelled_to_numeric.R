#' Labelled to numeric
#'
#' Converts a standard better / same / worse scale to numeric 1, 0, -1.
#' @param labelled_var  column from a survey data frame where gender is recorded.
#' @param labels The value labels
#' @param numeric_values The numeric values
#' @importFrom haven as_factor
#' @importFrom plyr mapvalues
#' @export

labelled_to_numeric <- function ( labelled_var,
                                  labels,
                                  numeric_values ) {

  if (length(labels) != length(numeric_values)) {
    stop("The original labels and the new numeric values do not match in lengths")
  }
  if ( class(labelled_var) == "labelled") {
    labelled_var <- haven::as_factor (labelled_var) }

  if ( class(labelled_var) %in% c("character", "factor") ) {
    labelled_var <- plyr::mapvalues(labelled_var,
                                    from  = labels,
                                    to = numeric_values)
  } else {
    #stop ("The variable is neither a character, factor or labelled variable.")
  }
  labelled_var <- as.character(labelled_var)
  labelled_var <- ifelse(labelled_var == "NA",
                         NA, labelled_var)
  return(as.numeric(labelled_var))
}
