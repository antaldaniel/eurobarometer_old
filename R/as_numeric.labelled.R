#' Convert to numeric from a GESIS labelled SPSS object
#'
#' @param x An object to be converted to numeric
#' @param type Any of \code{as_factor_binary}, \code{as_factor_3},
#'  \code{as_factor_pos_neg}, \code{as_factor_yes_no_4},
#' \code{rescale_time_frequency_use}, \code{rescale_alphanumeric}.
#' @examples
#' my_categories <- as_factor_3( c("Rural area or village",
#'  "Small or medium-sized town", "Inap.", "Large town/city",
#'   "DK", "Large town/city"))
#'
#' as_numeric (my_categories)
#'
#' @export
#'

as_numeric.labelled <- function(x, type = "as_factor_binary") {

  if (! haven::is.labelled(x)) {
    stop("Not a haven::labelled object.")
  }

  if (type == "as_factor_binary") {
    tmp <- as_factor_binary(x)
    return(tmp[[3]])
  }
  if (type == "as_factor_3") {
    tmp <- as_factor_3(x)
    return(tmp[[3]])
  }

  #if (type == "as_factor_pos_neg") {
  #  tmp <- as_factor_pos_neg(x)
  #  return(tmp[[3]])
  #}

  if (type == "as_factor_yes_no_4") {
    tmp <- as_factor_yes_no_4(x)
    return(tmp[[3]])
  }
  if (type == "rescale_time_frequency_use") {
    tmp <- rescale_time_frequency_use(x, return_class = "numeric")
    return(tmp(3))
  }
  if (type == "rescale_alphanumeric") {
    tmp <- rescale_alphanumeric_en(x, return_class = "numeric")
    return(tmp)
  }
  warning("No conversion took place.")
  return(tmp)
}

