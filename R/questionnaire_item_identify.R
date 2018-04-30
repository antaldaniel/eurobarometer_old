#' Identify questionnaire item
#'
#' @param x The original GESIS variable label (long)
#' @importFrom stringr str_split str_trim str_sub
#' @examples
#' \dontrun{
#' var_name_suggest ( gesis_name )
#' }
#' @export

questionnaire_item_identify <- function ( x ) {
 x <- tolower(x)
 word1 <- stringr::word(string = x, start = 1,end = 1, sep = " ")
 item <-  stringr::str_extract(string = word1, pattern = "[a-zA-Z]{1,2}\\d{1,2}[a-zA-Z]?")
 item
}
