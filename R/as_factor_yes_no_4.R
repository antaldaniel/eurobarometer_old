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
  x <- as.character (x)
  x <- tolower(x)
  voc <- eurobarometer::vocabulary
  voc <- lapply (voc, tolower)
  chr  <- ifelse ( x   %in% voc$pos_2, "absolutely_yes", x )
  chr  <- ifelse ( chr %in% voc$pos_1, "yes", chr )
  chr  <- ifelse ( chr %in% voc$neg_1,   "no", chr )
  chr  <- ifelse ( chr %in% voc$neg_2,   "absolutely_not", chr )
  chr  <- ifelse ( chr %in% c("yes", "no",
                            "absolutely_yes", "absolutely_not"),
                   as.character(chr), NA)

  num  <- plyr::mapvalues (chr, from = c("absolutely_not",
    "no", "yes", "absolutely_yes"),
                                 to = c(2,1,-1,-2))
  fct <- factor(chr, levels = c("absolutely_no",
                                "no", "yes", "absolutely_yes"))
  num <- as.numeric(num)

  value = list(x, fct, num)
  attr(value, "class") <- "factor_yes_no_4"
  value
}
