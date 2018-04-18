#' Suggest a new variable name
#'
#' @param x The original GESIS variable label (long)
#' @importFrom stringr str_split str_trim str_sub
#' @examples
#' \dontrun{
#' var_name_suggest ( gesis_name )
#' }
#' @export

var_name_suggest <- function ( x ) {

  x <- tolower(as.character(x))
  x <- gsub ( "%", "pct", x)
  x <- gsub ( "12\\+", "12p", x )
  x <- gsub ( "15\\+", "15p", x)
  x <- gsub ( "<10", "10m", x)
  x <- gsub ("\\.\\.\\.", "", x)
  x <- gsub ( "1st", "first", x)
  x <- gsub ( "2nd", "second", x)
  x <- gsub ( "3rd", "third", x)
  x <- gsub ( "4th", "fourth", x)
  x <- gsub ( "(spont)", "_spont", x)
  x <- gsub ( "(sum)", "_sum", x)
  x <- gsub ( "(summarized)", "_sum", x)
  x <- gsub ( "(summarised)", "_sum", x)
  x <- gsub ( "(recoded)", "_rec", x)
  x <- gsub ( "(rec)", "_rec", x)
  x <- gsub( " - ", ": ", x)
  x <- gsub( "\\s", "_", x)
  x <- gsub( ":_", ":", x)
  x <- gsub ( "/", "_", x)
  x <- gsub ( "\\(", "", x)
  x <- gsub ( ")", "", x)
  x <- gsub ( "\\+", "_", x)
  x <- gsub ( "\\&", "", x)
  x <- gsub ( "___", "_", x)
  x <- gsub ( "__", "_", x)
  x <- gsub ( "__", "_", x)
  x <- gsub ( "_-_", "_", x)
  x <- gsub ( "\\.", "_", x)
  x <- gsub ( ":", "_", x)
  x <- gsub ( "__", "_", x)
  x <- gsub ( "\u2026", "", x)
  x <- gsub ( "\\.", "", x)
  x <- gsub ( "\\.", "", x)
  x <- gsub ( "\\.", "", x)
  x <- gsub ( "_10mm", "_10m", x)
  x <- gsub ( "__", "_", x)
  x <- ifelse ( stringr::str_sub(x, 1,1) == "_",
                stringr::str_sub(x, 2,-1),
                x)
  x <- ifelse ( stringr::str_sub(x, -1,-1) == "_",
                stringr::str_sub(x, 1,-2),
                x)
  x <- gsub ( "__", "_", x)

  x <- gsub("_10p-scale", "", x)
  x <- gsub("aged_<10", "aged_10m", x )
  x <- gsub("_aged_15_", "aged_15p", x )
  x <- gsub("aged_10-14", "aged_10_14", x )
  x <- gsub("di_rection_", "direction_", x)

  x = stringr::str_trim(x, side = "both")

  x = ifelse ( grepl( "w1_weight_result", tolower(x)),
               yes = "w1",
               no = x )

  x = ifelse ( grepl( "all_samples_iso_3166", tolower(x)),
               yes = "country_code_iso_3166",
               no = x )

  x = ifelse ( grepl( "date_of_interview", tolower(x)),
               yes = "date_of_interview",
               no = x )
  x = ifelse (
    stringr::str_sub(x, 1,14) == "mc_nationality",
    yes = gsub("mc_", "", x),
    no = x)
  x
  }
