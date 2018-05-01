#' GESIS shortname harmonization
#'
#' Using the data in the short variable names (codes) to correctly assign a
#' long variable name
#' @param short_name The metadata$spss_name
#' @param long_name  The updated metadata$suggested_name
#' @examples
#' \dontrun{
#'  gesis_shortname_harmonization (short_name = "wextra",
#'                                 long_name  = "Weight extrapolated 15+")
#' }
#' @export

gesis_shortname_harmonization <- function (short_name, long_name ) {

  long_name = ifelse (
    short_name == "split",
    yes = "split",
    no = long_name)
  long_name = ifelse (
    short_name == "uniqid",
    yes = "uniqid",
    no = long_name)
  long_name = ifelse(
    short_name == "wex",
    yes = "wex",
    no = long_name )
  long_name = ifelse(
    short_name == "wextra",
    yes = "wex",
    no = long_name )
  long_name = ifelse (
    tolower(short_name) == "w1",
    yes = "w1",
    no = long_name )

  long_name
}
