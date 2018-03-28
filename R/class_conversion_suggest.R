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

  if (class(x) %in% c("numeric", "integer")) return("numeric")
  if (class(x) %in% c("character")) return("character")
  if (class(x) %in% c("logical")) return("logical")
  if (class(x) %in% c("labelled")) {

    x <- as.character(haven::as_factor(x))
    unique_values <- tolower(as.character(x))

    unique_values <- unique (unique_values)[ ! unique(x) %in% c(NA, "dk", "refusal")]
    unique_values <- ifelse (grepl("inap", tolower(unique_values)), NA,  unique_values)
    unique_values <- unique(unique_values[!is.na(unique_values)])

    if ( length ( unique_values) == 2) {
      if ( sum(unique_values == c("not mentioned", "dk"))==2 ) return(as.character("indicator"))
      if ( sum(unique_values == c("dk", "not mentioned"))==2) return(as.character("indicator"))
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

     if ( sum( unique_values %in%
               tolower(values_factor_binary$positive_values)) == 1 ) {
       if ( sum( unique_values %in%
                 tolower(values_factor_binary$negative_values)) == 1 ) {
         return(as.character("factor_binary"))
       }
     }
    }
    ## Three unique values values
    if ( length (unique_values) == 3 ) {
     for ( i in 1:nrow(values_factor_3)) {
       a <- sum ( unique_values %in% tolower(values_factor_3$factor_3_0[i]))
       b <- sum ( unique_values %in% tolower(values_factor_3$factor_3_1[i]))
       c <- sum ( unique_values %in% tolower(values_factor_3$factor_3_2[i]))
    if ( (a+b+c) == 3 ) return(as.character("factor_3"))
        }
        for ( i in 1:nrow(values_factor_pos_neg))  {
          a <- sum ( unique_values %in% tolower(values_factor_pos_neg$factor_positive[i]))
          b <- sum ( unique_values %in% tolower(values_factor_pos_neg$factor_null[i]))
          c <- sum ( unique_values %in% tolower(values_factor_pos_neg$factor_negative[i]))
          if ( (a+b+c) == 3 ) return(as.character("factor_pos_neg"))
        }
    }
      ## Four unique values values
      if ( length (unique_values) == 4 ) {
        for ( i in 1:nrow(values_factor_4)) {
          a <- sum ( unique_values %in% tolower(values_factor_4$factor_4_0[i]))
          b <- sum ( unique_values %in% tolower(values_factor_4$factor_4_1[i]))
          c <- sum ( unique_values %in% tolower(values_factor_4$factor_4_2[i]))
          d <- sum ( unique_values %in% tolower(values_factor_4$factor_4_3[i]))
          if ( (a+b+c+d) == 4 ) return(as.character("factor_4"))
        }
          for ( i in 1:nrow(values_factor_yes_no_4))  {
            a <- sum ( unique_values %in% tolower(values_factor_yes_no_4$yes_2_value[i]))
            b <- sum ( unique_values %in% tolower(values_factor_yes_no_4$yes_1_value[i]))
            c <- sum ( unique_values %in% tolower(values_factor_yes_no_4$no_1_value[i]))
            d <- sum ( unique_values %in% tolower(values_factor_yes_no_4$no_2_value[i]))
            if ( (a+b+c+d) == 4 ) return(as.character("factor_yes_no_4"))
          }
        }

        ## Five unique values values
        if ( length (unique_values) == 5 ) {
          for ( i in 1:nrow(values_factor_5)) {
            a <- sum ( unique_values %in% tolower(values_factor_5$factor_5_0[i]))
            b <- sum ( unique_values %in% tolower(values_factor_5$factor_5_1[i]))
            c <- sum ( unique_values %in% tolower(values_factor_5$factor_5_2[i]))
            d <- sum ( unique_values %in% tolower(values_factor_5$factor_5_3[i]))
            e <- sum ( unique_values %in% tolower(values_factor_5$factor_5_4[i]))
            if ( (a+b+c+d+e) == 5 ) return(as.character("factor_5"))
          }
        }
  } #end of class labelled
  return(as.character("factor"))
}
