#' Get the recoding vocabulary
#'
#' Get the vocabulary used for re-coding or reordering
#' categorical variables
#' @importFrom type Filtering for the vocabulary of a class only, such as the
#' vocabulary of only 3-level \code{factor_3} variables. Defaults to \code{NULL}.
#' @importFrom dplyr filter
#' @importFrom magrittr '%>%'
#' @examples
#' vocabulary_items_get()
#' @export

vocabulary_items_get <- function ( type = NULL) {
  if (is.null(type)) { return (vocabulary) }
  if (type %in% unique (vocabulary$context)) {
    voc <- vocabulary %>%
      dplyr::filter ( context ==  !! type  )
    return (voc)
  } else {
    stop("Error: ", type, " not recognized in the vocabulary file.")
  }
}



