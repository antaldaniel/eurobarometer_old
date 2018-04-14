#' Constructor for factor_pos_neg
#'
#' @param x An input vector to be converted to a
#' three level (positive, neutral, negative) variable class.
#' @importFrom plyr mapvalues
#' @importFrom haven as_factor
#' @examples
#' as_factor_pos_neg( c("Better", "DK", "Worse",
#'                    "Same", "The Same", "Inap. not"))
#'
#' @export

as_factor_pos_neg <- function(x) {
  if ( "labelled" %in% class(x)) {
    x <- haven::as_factor(x)
  }
  x <- as.character (x)
  x <- tolower(x)
  voc <- vocabulary_items_get ( context_var = "factor_pos_neg")
  voc <- lapply (voc, tolower)
  pos <- tolower(voc$pos_1)
  nul <- tolower(voc$neutral)
  neg <-  tolower(voc$neg_1)
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
