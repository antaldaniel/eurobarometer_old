#' Get the recoding vocabulary
#'
#' Get the vocabulary used for re-coding or reordering
#' categorical variables
#' @param context Filtering for the vocabulary of a class only, such as the
#' vocabulary of only 3-level \code{factor_3} variables. Defaults to \code{NULL}.
#' @importFrom dplyr filter
#' @importFrom magrittr '%>%'
#' @examples
#' vocabulary_items_get()
#' @export

vocabulary_items_get <- function ( context = NULL) {
  if (is.null(context)) { return (vocabulary) }
  if (context %in% unique (vocabulary$context)) {
    voc <- vocabulary %>%
      dplyr::filter ( context ==  !! context  )
    return (voc)
  } else {
    stop("Error: ", context, " not recognized in the vocabulary file.")
  }
}



