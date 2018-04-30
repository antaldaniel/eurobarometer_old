#' Constructor for quasi-numeric frequency factors
#'
#' @param x An input vector to be converted to time frequencies
#' @importFrom plyr mapvalues
#' @importFrom haven as_factor
#' @examples
#' as_factor_frequency( c("Less often", "Several times a month",
#'                "Every 2-3 months", "Once a month", "DK"))
#'
#' @export

as_factor_frequency <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- haven::as_factor(x)
  }
  x <- as.character (x)
  x <- tolower(x)
  voc <- vocabulary_items_get ( context_var = "factor_frequency")
  voc <- lapply (voc, tolower)
  factor_freq_36 <- tolower(voc[['36']])
  factor_freq_12 <- tolower(voc[['12']])
  factor_freq_6  <- tolower(voc[['6']])
  factor_freq_3  <- tolower(voc[['3']])
  factor_freq_0  <- tolower(voc[['0']])

  num  <- ifelse ( x   %in% factor_freq_36, "36", x )
  num  <- ifelse ( num %in% factor_freq_12, "12", num )
  num  <- ifelse ( num %in% factor_freq_6, "6", num )
  num  <- ifelse ( num %in% factor_freq_3, "3", num )
  num  <- ifelse ( num %in% c("36", "12", "6" ,"3"),
                   as.character(num), NA)
  num <- as.numeric(num)

  unique_values <- unique(x)

  chr_36 <- unique_values [ which(unique_values %in% factor_freq_36)]
  chr_12 <- unique_values [ which(unique_values %in% factor_freq_12)]
  chr_6  <- unique_values [ which(unique_values %in% factor_freq_6)]
  chr_3  <- unique_values [ which(unique_values %in% factor_freq_3)]
  chr   <- plyr::mapvalues (num, from = c(36,12,6,3),
                                 to = c(chr_36,
                                        chr_12,
                                        chr_6,
                                        chr_3))
  fct <- factor(chr, levels = c(chr_36,chr_12,chr_6,chr_3))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_frequency"
  value
}
