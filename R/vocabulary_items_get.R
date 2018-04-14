#' Get the recoding vocabulary
#'
#' Get the vocabulary used for re-coding or reordering
#' categorical variables
#' @param context_var Filtering for the vocabulary of a class only, such as the
#' vocabulary of only 3-level \code{factor_3} variables. Defaults to \code{NULL}.
#' @importFrom dplyr filter
#' @importFrom magrittr '%>%'
#' @examples
#' vocabulary_items_get()
#' @export

vocabulary_items_get <- function ( context_var = NULL) {
  context <- NULL
  voc <- eurobarometer::vocabulary
  if (is.null(context_var)) { return (voc) }
  if (context_var %in% unique (voc$context)) {
    voc <- voc %>%
      dplyr::filter ( context ==  context_var )
    return (voc)
  } else {
    stop("Error: ", context_var, " not recognized in the vocabulary file.")
  }
}



