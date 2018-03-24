#' A helper function to separate GESIS variable name parts.
#'
#' @param x A variable to summarize.
#' @examples
#' \dontrun{
#' get_eb_questionnaire_item <- function ( x )
#' }
#' @export

get_eb_questionnaire_item <- function ( x ) {

  gesis_2 <- stringr::str_split_fixed(x, " ", 2)
  first_word <- gesis_2[,1]
  eb_item <- vector(mode='character', length(first_word))
  eb_item <- ifelse ( grepl ( "^[A-Za-z]{2}\\d{1,2}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^P[0-9]{1}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^P[0-9]{2}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^Q{1}\\d{1,2}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^C{1}\\d{1,2}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^D{1}\\d{1,2}", first_word),
                      first_word, eb_item)
  eb_item <- ifelse ( grepl ( "^W{1}\\d{1,2}", first_word),
                      first_word, eb_item)
  eb_item

  return_df = data.frame (
    questionnaire_item = eb_item,
    gesis_name = x,
    stringsAsFactors = FALSE
  )

  for ( i in 1:nrow(gesis_2)) {
    if ( return_df$questionnaire_item[i] !="") {
      return_df$gesis_name[i] <- stringr::str_trim(
               gsub(return_df$questionnaire_item[i],
                    "", return_df$gesis_name[i]
                    ), side= "both") }

  }

  return(return_df)

}

