#' Get the recoding vocabulary
#'
#' Get the vocabulary used for re-coding or reordering
#' categorical variables
#' @examples
#' vocabulary_items_get()
#' @export

vocabulary_items_get <- function ( type = NULL) {
  load(vocabulary)

  return (vocabulary)
}



