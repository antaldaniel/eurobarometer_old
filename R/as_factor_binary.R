#' Constructor for factor_binary
#'
#' @param x An input vector to be converted to binary factor class.
#' @importFrom plyr mapvalues
#' @importFrom haven as_factor
#' @examples
#' as_factor_binary(c("mentioned", "mentioned",
#'                    "not mentioned", "dk"))
#'
#' as_factor_binary ( c("yes", "no", "DK", "Inap."))
#'
#' @export

as_factor_binary <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- as.character(haven::as_factor(x))
  }
  if ( class(x)[1] == "character" ) {
    x <- as.character (x)
  }

  x <- tolower(x)
  binary_values <- binary_values_get()
  yes_values <- binary_values[[1]]
  no_values  <- binary_values[[2]]
  unique_binary_values <- unique(x)
  yes_value <- yes_values[ which(yes_values %in% unique_binary_values)]
  no_value  <-  no_values[ which( no_values %in% unique_binary_values)]

  num  <- ifelse ( x %in% yes_value, "1", x )
  num  <- ifelse ( num %in% no_value, "0", num)
  num  <- ifelse ( num %in% c("1", "0"), as.character(num), NA)

  chr  <- plyr::mapvalues (num, from = c("1", "0"),
                                 to = c(yes_value,no_value))
  fct <- factor(chr, levels = c(no_value,yes_value))
  num <- as.numeric(num)
  value = list(chr, fct, num)
  attr(value, "class") <- "factor_binary"
  value
}

