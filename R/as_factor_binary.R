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
  voc <- vocabulary_items_get( context_var = "factor_binary")
  voc <- lapply(voc, tolower)
  yes_values <- tolower(voc$pos_1)
  no_values  <- tolower(voc$neg_1)
  unique_binary_values <- unique(x)
  if (sum(c("false (correct)", "true", "dk") %in% unique_binary_values) == 3) {
    yes_value <- "false (correct)"
    no_value <- "true"
  } else if (sum(c("true (correct)", "false", "dk") %in% unique_binary_values) == 3) {
    yes_value <- "true (correct)"
    no_value <- "false"
  } else {
    yes_value <- yes_values[ which(yes_values %in% unique_binary_values)]
    no_value  <- no_values[ which( no_values %in% unique_binary_values)]

  }
   if ( length (no_value)>1 ) {
    if (length(unique(no_value)) == 1 ) {
      no_value = unique(no_value) }
    else ( stop ( "Not a unique no value."))
  }

  if ( length (yes_value)>1 ) {
    if (length(unique(yes_value)) == 1 ) {
      no_value = unique(yes_value) }
    else ( stop ( "Not a unique yes value."))
  }

  if (length(yes_value) == 0 ) { stop("Yes value was not found in binary factor.")}
  if (length(no_value ) == 0 ) { stop("No value was not found in binary factor.")}
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

