#' Add standard NUTS2 codes.
#'
#' Currently works only with NUTS2010 (for years 2012-2014) and NUTS2013
#' (for years 2015-2017). In the case of earlier and later data sets
#' boundary changes may effect some regions. See
#' [Eurostat History of NUTS](http://ec.europa.eu/eurostat/web/nuts/history).
#' Small countries where Eurobarometer has NUT3 level data are aggregated
#' to NUTS2 (Croatia, Ireland, Latvia, Lithuania, Slovenia). Malta, Cyprus
#' and Luxembourg receive an auxilliary code MT00, CY00, LU00.
#' In the United Kingdom, NUTS2 level data is available only for Northern
#' Ireland. The country code of the United Kingdom in the GESIS files is
#' GB, but in the NUTS2 vocubulary it is UK.
#' In Germany, France, Italy and the United Kingdom only NUTS1 level data
#' is available.
#' The region codes of Greece (GR) start with EL.
#' @param region_nuts_codes A region_nuts_codes column from a GESIS file.
#' @param country_code To avoid ambigouity with NUTS name coding, provide country
#' codes, if applicable. There are a few ambigous names in Europe.
#' @param nuts_code Currently only \code{code2010} and \code{code2013}
#' is supported.
#' @importFrom dplyr left_join mutate_all
#' @examples
#' code_nuts2  (region_nuts_codes = c("Tirol", "Praha", NA))
#'
#' code_nuts2  (region_nuts_codes = c("Tirol", "Praha", "MALTA"))
#'
#' @export

code_nuts2 <- function ( country_code = NULL,
                         region_nuts_codes,
                         nuts_code = "code2010")  {

  if (! nuts_code %in% c("code2010", "code2013")) {
    stop("Currently only NUTS2010 (for years 2012-2014)
         and NUTS2013 (for years 2015-2017) can be coded.")
  }

 nuts2 <- vector ( mode = "character", length = length(region_nuts_codes))

 if ( is.null(country_code) ) {
   vocabulary <- eurobarometer::vocabulary_nuts2 %>%
     mutate (region_nuts_codes = tolower(as.character(region_nuts_codes)))
   df <- data.frame ( region_nuts_codes = tolower(as.character(region_nuts_codes)),
                      stringsAsFactors = FALSE)
   df$row <- 1:nrow(df)
   nuts2_df <- dplyr::left_join ( df, vocabulary,
                                  by = "region_nuts_codes") %>%
     add_count( row ) #check there are not duplicates
 } else {
   vocabulary <- eurobarometer::vocabulary_nuts2 %>%
     mutate ( country_code = tolower(as.character(country_code)),
              region_nuts_codes = tolower(as.character(region_nuts_codes)))
   df <- data.frame ( country_code = tolower(country_code),
                      region_nuts_codes = tolower(as.character(region_nuts_codes)),
                      stringsAsFactors = FALSE)
   df$row <- 1:nrow(df)
   nuts2_df <- dplyr::left_join ( df, vocabulary,
                                  by = c("country_code",
                                         "region_nuts_codes")
                                  ) %>%
     add_count( row ) #check there are not duplicates

 }
 if ( any(nuts2_df$n > 1) ) {
   warning("Duplicate region names found!")
 }

 if (nuts_code == "code2010") return (  nuts2_df$code2010 )
 if (nuts_code == "code2013") return (  nuts2_df$code2013 )

}
