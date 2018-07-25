#' Add standard NUTS2 codes.
#'
#' Currently works only with NUTS2010 (for years 2012-2014) and NUTS2013
#' (for years 2015-2017). In the case of earlier and later data sets
#' boundary changes may effect some regions. See
#' [Eurostat History of NUTS](http://ec.europa.eu/eurostat/web/nuts/history)
#' @param region_nuts_codes A region_nuts_codes column from a GESIS file.
#' @param country_code Default is \code{NULL}, if provided, small countries,
#' which have no NUT2 regions due to their size,  will be projected as
#'  NUTS2 regions, for example, Luxembourg with code LU00.
#' @param nuts_code Currently only \code{code2010} and \code{code2013}
#' is supported.
#' @importFrom dplyr left_join
#' @examples
#' code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA),
#'             country_code = c("AT", "CZ", "CY"))
#'
#'code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA))
#'
#' @export

code_nuts2 <- function ( region_nuts_codes,
                         country_code = NULL,
                         nuts_code = "code2010")  {

  if (! nuts_code %in% c("code2010", "code2013")) {
    stop("Currently only NUTS2010 (for years 2012-2014)
         and NUTS2013 (for years 2015-2017) can be coded.")
  }

 nuts2 <- vector ( mode = "character", length = length(region_nuts_codes))

 if (!is.null(country_code)) {
   df <- data.frame ( country_code, region_nuts_codes, nuts2,
                      stringsAsFactors = FALSE)
   nuts2 <- dplyr::left_join ( df, vocabulary_nuts2,
                               by = c("region_nuts_codes", "country_code")
                               )
   #project small countries to NUTS2
   nuts2$code2010 <- ifelse ( country_code %in% c(
     "LT", "LU", "LV", "MT", "CY", "EE"   #review ME, IS, etc.
   ),
   yes = paste0(country_code, "00"),
   no = nuts2$code2010)
 } else {
   df <- data.frame ( region_nuts_codes, nuts2,
                      stringsAsFactors = FALSE)
   nuts2 <- dplyr::left_join ( df, vocabulary_nuts2,
                               by = "region_nuts_codes")

 }

 if (nuts_code == "code2010") return ( nuts2$code2010 )
 if (nuts_code == "code2013") return ( nuts2$code2013 )

}
