#' Constructor for factor_3 three positive-level factor
#'
#' @param x An input vector to be converted to a
#' three positive level variable
#' @importFrom plyr mapvalues
#' @examples
#' as_factor_3( c("Rural area or village", "Small or medium-sized town",
#'                   "Inap.", "Large town/city", "DK", "Large town/city"))
#'
#' @export

as_factor_3 <- function(x) {
  x <- as.character (x)
  x <- tolower(x)
  factor_3_list <- values_factor_3_get()
  factor_3_0 <- tolower(factor_3_list[[1]])
  factor_3_1 <- tolower(factor_3_list[[2]])
  factor_3_2 <- tolower(factor_3_list[[3]])

  num  <- ifelse ( x   %in% factor_3_0, "0", x )
  num  <- ifelse ( num %in% factor_3_1, "1", num )
  num  <- ifelse ( num %in% factor_3_2, "2", num )
  num  <- ifelse ( num %in% c("0", "1", "2"),
                   as.character(num), NA)
  num <- as.numeric(num)

  unique_values <- unique(x)

  chr_0 <- unique_values [ which(unique_values %in% factor_3_0)]
  chr_1 <- unique_values [ which(unique_values %in% factor_3_1)]
  chr_2 <- unique_values [ which(unique_values %in% factor_3_2)]
  chr   <- plyr::mapvalues (num, from = c(0,1,2),
                                 to = c(chr_0,chr_1,chr_2))
  fct <- factor(chr, levels = c(chr_0,chr_1,chr_2))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_3"
  value
}
