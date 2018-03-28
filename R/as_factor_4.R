#' Constructor for factor_4 four non-negative level factor
#'
#' @param x An input vector to be converted to a
#' four non-negative level variable
#' @importFrom plyr mapvalues
#' @importFrom haven as_factor
#' @examples
#' as_factor_4( c("Not at all", "Low",
#'                "Medium", "Strong", "DK"))
#'
#' @export

as_factor_4 <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- haven::as_factor(x)
  }
  x <- as.character (x)
  x <- tolower(x)
  factor_4_0 <- tolower(values_factor_4$factor_4_0)
  factor_4_1 <- tolower(values_factor_4$factor_4_1)
  factor_4_2 <- tolower(values_factor_4$factor_4_2)
  factor_4_3 <- tolower(values_factor_4$factor_4_3)
  num  <- ifelse ( x   %in% factor_4_0, "0", x )
  num  <- ifelse ( num %in% factor_4_1, "1", num )
  num  <- ifelse ( num %in% factor_4_2, "2", num )
  num  <- ifelse ( num %in% factor_4_3, "3", num )
  num  <- ifelse ( num %in% c("0", "1", "2" ,"3"),
                   as.character(num), NA)
  num <- as.numeric(num)

  unique_values <- unique(x)

  chr_0 <- unique_values [ which(unique_values %in% factor_4_0)]
  chr_1 <- unique_values [ which(unique_values %in% factor_4_1)]
  chr_2 <- unique_values [ which(unique_values %in% factor_4_2)]
  chr_3 <- unique_values [ which(unique_values %in% factor_4_3)]
  chr   <- plyr::mapvalues (num, from = c(0,1,2,3),
                                 to = c(chr_0,
                                        chr_1,
                                        chr_2,
                                        chr_3))
  fct <- factor(chr, levels = c(chr_0,chr_1,chr_2,chr_3))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_4"
  value
}
