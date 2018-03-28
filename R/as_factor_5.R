#' Constructor for factor_5 four non-negative level factor
#'
#' @param x An input vector to be converted to a
#' four non-negative level variable
#' @importFrom plyr mapvalues
#' @importFrom haven as_factor
#' @examples
#' as_factor_5( c("Much lower", "Somewhat lower",
#' 	"More or less the same", 	"Somewhat higher", "Much higher"))
#'
#' @export

as_factor_5 <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- haven::as_factor(x)
  }
  x <- as.character (x)
  x <- tolower(x)
  factor_5_0 <- tolower(values_factor_5$factor_5_0)
  factor_5_1 <- tolower(values_factor_5$factor_5_1)
  factor_5_2 <- tolower(values_factor_5$factor_5_2)
  factor_5_3 <- tolower(values_factor_5$factor_5_3)
  factor_5_4 <- tolower(values_factor_5$factor_5_4)
  num  <- ifelse ( x   %in% factor_5_0, "0", x )
  num  <- ifelse ( num %in% factor_5_1, "1", num )
  num  <- ifelse ( num %in% factor_5_2, "2", num )
  num  <- ifelse ( num %in% factor_5_3, "3", num )
  num  <- ifelse ( num %in% factor_5_4, "4", num )
  num  <- ifelse ( num %in% c("0", "1", "2",
                              "3", "4"),
                   as.character(num), NA)
  num <- as.numeric(num)

  unique_values <- unique(x)

  chr_0 <- unique_values [ which(unique_values %in% factor_5_0)]
  chr_1 <- unique_values [ which(unique_values %in% factor_5_1)]
  chr_2 <- unique_values [ which(unique_values %in% factor_5_2)]
  chr_3 <- unique_values [ which(unique_values %in% factor_5_3)]
  chr_4 <- unique_values [ which(unique_values %in% factor_5_4)]
  chr   <- plyr::mapvalues (num, from = c(0,1,2,3,4),
                                 to = c(chr_0,
                                        chr_1,
                                        chr_2,
                                        chr_3,
                                        chr_4))
  fct <- factor(chr, levels = c(chr_0,chr_1,chr_2,
                                chr_3, chr_4))
  value = list(x, fct, num)
  attr(value, "class") <- "factor_5"
  value
}
