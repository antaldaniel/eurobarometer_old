#' Get binary yes values
#'
#' Get the character strings that will be recoded as "yes" in
#' binary variables.
#' @examples
#' binary_values_get()
#'
#' @export
#'

binary_values_get <- function () {
    return(values_factor_binary)
}
