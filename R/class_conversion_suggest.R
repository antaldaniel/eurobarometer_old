#' Suggest variable conversion
#'
#' @param x A variable from a GESIS archive.
#' @importFrom haven as_factor
#' @examples
#' \dontrun{
#' suggest_variable_conversion (x)
#' }
#' @export

class_conversion_suggest <- function (x) {

  if ("numeric" %in% class(x) ||
      "integer" %in% class(x))   return("numeric")
  if ("factor" %in% class(x))    return("factor")
  if ("logical" %in% class(x))   return("logical")
  if ("character" %in% class(x)) return("character")

  if ("labelled" %in% class(x)) {

    x <- as.character(haven::as_factor(x))
    unique_values <- tolower(as.character(x))
    unique_values <- unique (unique_values)[ ! unique(x) %in% c(NA, "dk", "refusal")]
    unique_values <- ifelse (grepl("inap", tolower(unique_values)), NA,  unique_values)
    unique_values <- unique(unique_values[!is.na(unique_values)])

    if ( length (unique_values) == 2) {
      if ( sum(unique_values == c("not mentioned", "dk"))==2 ) return(as.character("indicator"))
      if ( sum(unique_values == c("dk", "not mentioned"))==2 ) return(as.character("indicator"))
    }

    unique_values <- unique_values[which(unique_values != "dk")]
    unique_values <- gsub(" \\(spontaneous\\)", "", unique_values)
    unique_values <- gsub(" \\(spont\\)", "", unique_values)
    unique_values <- gsub("true \\(correct\\)", "true", unique_values)
    unique_values <- gsub("true \\[correct\\]", "true", unique_values)
    unique_values <- gsub("true\\.", "true", unique_values)
    unique_values <- gsub("false \\(correct\\)", "false", unique_values)
    unique_values <- gsub("false \\[correct\\]", "false", unique_values)
    unique_values <- gsub("false\\.", "false", unique_values)
    #unique_values
    ## Two identical values
    if ( length (unique_values) == 2 ) {
      if ( "not mentioned" %in% unique_values ) {
        if ( "mentioned" %in% unique_values ) {
          return(as.character("multiple_choice")) }
        else { return(as.character("indicator"))  }
      }

   ##Detect binary factors
    voc <- vocabulary_items_get ( context_var = "factor_binary" )

     if ( sum( unique_values %in%
               tolower(voc$pos_1)) == 1 ) {
       if ( sum( unique_values %in%
                 tolower(voc$neg_1)) == 1 ) {
         return(as.character("factor_binary")) }
     }
    }

    ## Three unique values values
    if ( length (unique_values) == 3 ) {
      voc <- vocabulary_items_get ( context_var = "factor_3" )
     for ( i in 1:nrow(voc)) {
       a <- sum ( unique_values %in% tolower(voc$pos_1[i]))
       b <- sum ( unique_values %in% tolower(voc$pos_2[i]))
       c <- sum ( unique_values %in% tolower(voc$neutral[i]))
       if ( (a+b+c) == 3 ) return(as.character("factor_3"))
       }
      voc <- vocabulary_items_get ( context_var = "factor_pos_neg" )
      for ( i in 1:nrow(voc))  {
        a <- sum ( unique_values %in% tolower(voc$pos_1[i]))
        b <- sum ( unique_values %in% tolower(voc$neutral[i]))
        c <- sum ( unique_values %in% tolower(voc$neg_1[i]))
        if ( (a+b+c) == 3 ) return(as.character("factor_pos_neg"))
      }
    }
      ## Four unique values values
      if ( length (unique_values) == 4 ) {

        voc <- vocabulary_items_get ( context_var = "factor_frequency" )
        for ( i in 1:nrow(voc)) {
          a <- sum ( unique_values %in% tolower(voc[['36']]))
          b <- sum ( unique_values %in% tolower(voc[['12']]))
          c <- sum ( unique_values %in% tolower(voc[['6']]))
          d <- sum ( unique_values %in% tolower(voc[['3']]))
          if ( (a+b+c+d) == 4 ) return(as.character("factor_frequency"))
        }

        voc <- vocabulary_items_get ( context_var = "factor_4" )
        for ( i in 1:nrow(voc)) {
          a <- sum ( unique_values %in% tolower(voc$neutral[i]))
          b <- sum ( unique_values %in% tolower(voc$pos_1[i]))
          c <- sum ( unique_values %in% tolower(voc$pos_2[i]))
          d <- sum ( unique_values %in% tolower(voc$pos_3[i]))
          if ( (a+b+c+d) == 4 ) return(as.character("factor_4"))
        }
        voc <- vocabulary_items_get ( context_var = "yes_no_4" )
          for ( i in 1:nrow(voc))  {
            a <- sum ( unique_values %in% tolower(voc$pos_2[i]))
            b <- sum ( unique_values %in% tolower(voc$pos_1[i]))
            c <- sum ( unique_values %in% tolower(voc$neg_1[i]))
            d <- sum ( unique_values %in% tolower(voc$neg_2[i]))
            if ( (a+b+c+d) == 4 ) return(as.character("factor_yes_no_4"))
          }
        }

        ## Five unique values values
        if ( length (unique_values) == 5 ) {
          voc <- vocabulary_items_get ( context_var = "factor_5" )
          for ( i in 1:nrow(voc)) {
            a <- sum ( unique_values %in% tolower(voc$neg_2[i]))
            b <- sum ( unique_values %in% tolower(voc$neg_1[i]))
            c <- sum ( unique_values %in% tolower(voc$neutral[i]))
            d <- sum ( unique_values %in% tolower(voc$pos_1[i]))
            e <- sum ( unique_values %in% tolower(voc$pos_2[i]))
            if ( (a+b+c+d+e) == 5 ) return(as.character("factor_5"))
          }
        }
  } #end of class labelled
  return(as.character("factor"))
}
