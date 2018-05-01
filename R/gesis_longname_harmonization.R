#' GESIS long name harmonization
#'
#' Using the data in the short variable names (codes) to correctly assign a
#' long variable name
#' @param long_name  The updated metadata$suggested_name
#' @examples
#' \dontrun{
#'  gesis_shortname_harmonization (long_name  = "Weight extrapolated 15+")
#' }
#' @export


gesis_longname_harmonization <- function (long_name) {
  naming_exceptions <- get_naming_exceptions()
  patterns <- tolower(naming_exceptions$exact)

  pattern <- paste(naming_exceptions$exact, collapse = "|") # create regex pattern


 return_df <-  data.frame(
   exact = long_name,
   stringsAsFactors = F
 ) %>% left_join (., naming_exceptions, by = "exact")

 if (sum(grepl(pattern, long_name)) > sum(!is.na(return_df$new_name))) {
   ##New partial matches can improve the naming
   for ( i in 1:nrow(naming_exceptions)) {
     pattern = naming_exceptions$exact[i]
     new_name = naming_exceptions$new_name[i]
     return_df$new_name <- gsub(pattern, new_name, return_df$new_name)
   }

 }
 return_df$new_name <- ifelse (is.na(return_df$new_name),
                               var_name_suggest(return_df$exact),
                               return_df$new_name)
 return_df$new_name
}

