#' Constructor for factor_yes_no_4
#'
#' @param x An input vector to be converted to a four level yes-no variable
#' @importFrom plyr mapvalues
#' @examples
#' as_factor_pos_neg( c("Better", "DK", "Worse",
#'                    "Same", "The Same", "Inap. not"))
#'
#' @export

as_factor_pos_neg <- function(x) {
  x <- as.character (x)
  x <- tolower(x)
  values_pos_neg <- values_pos_neg_get()
  pos <- tolower(values_pos_neg[[1]])
  nul <- tolower(values_pos_neg[[2]])
  neg <-  tolower(values_pos_neg[[3]])
  chr  <- ifelse ( x   %in% pos, "positive", x )
  chr  <- ifelse ( chr %in% nul, "neutral", chr )
  chr  <- ifelse ( chr %in% neg, "negative", chr )
  chr  <- ifelse ( chr %in% c("positive", "neutral",
                            "negative"),
                   as.character(chr), NA)

  num  <- plyr::mapvalues (chr, from = c("positive", "neutral",
                                         "negative"),
                                 to = c(1,0,-1))
  fct <- factor(chr, levels = c("negative", "neutral",
                                "positive"))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_pos_neg"
  value
}