#' Constructor for factor_yes_no_4
#'
#' @param x An input vector to be converted to a four level yes-no variable
#' @importFrom plyr mapvalues
#' @importFrom utils data
#' @examples
#' as_factor_yes_no_4( c("Very good", "Rather Good",
#'                   "Inap.", "Rather Bad", "DK", "Very bad"))
#'
#' @export

as_factor_yes_no_4 <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- haven::as_factor(x)
  }
  x <- as.character (x)
  x <- tolower(x)
  voc <- vocabulary_items_get ( context_var = "yes_no_4")
  voc <- lapply (voc, tolower)
  unique_values <- unique(x)

  num  <- ifelse ( x   %in% voc$pos_2, 2, x )
  num  <- ifelse ( num %in% voc$pos_1, 1, num )
  num  <- ifelse ( num %in% voc$neg_1, -1, num )
  num  <- ifelse ( num %in% voc$neg_2, -2, num )
  num  <- ifelse ( num %in% c(2,1,-1,-2),
                   as.character(num), NA)
  chr <- num

  chr_1 <- unique_values [ which(unique_values %in% voc$neg_2) ]
  chr_2 <- unique_values [ which(unique_values %in% voc$neg_1) ]
  chr_3 <- unique_values [ which(unique_values %in% voc$pos_1) ]
  chr_4 <- unique_values [ which(unique_values %in% voc$pos_2) ]

  chr[which ( num == -2)] <- chr_1
  chr[which ( num == -1)] <- chr_2
  chr[which ( num ==  1)] <- chr_3
  chr[which ( num ==  2)] <- chr_4

  fct <- factor(chr, levels = c(chr_1,chr_2,chr_3,chr_4))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_yes_no_4"
  value
}
