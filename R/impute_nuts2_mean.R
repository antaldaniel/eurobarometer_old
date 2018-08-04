#' Impute NUTS2 regional mean
#'
#' @param df  column from a survey data frame where gender is recorded.
#' @param impute_countries The value labels
#' @param weight_var The weighting variable in \code{df}. Defaults to \code{NULL}.
#' @param nuts_code Currently only \code{code2010} and \code{code2013}
#' is supported.
#' @importFrom dplyr mutate_if mutate select filter left_join ungroup group_by
#' @examples
#' sample  <- gesis_sample[,-c(1,3,10)]
#' impute_nuts2_mean(sample, nuts_code = "code2010")
#'

#' @export

impute_nuts2_mean <- function (df,
                               impute_countries = c("DE","IT", "UK"),
                               weight_var = NULL,
                               nuts_code = "code2010") {

  if (! nuts_code %in% c("code2010", "code2013")) {
    stop("Currently only NUTS2010 (for years 2012-2014)
         and NUTS2013 (for years 2015-2017) can be coded.")
  }

  if ( is.null(weight_var) ) {
    weights <- rep(1, nrow(df) )
  } else if ( weight_var %in% df ) {
    weights <- gesis[, which(names(gesis)==weight_var)]
  } else {
    weights <- rep(1, nrow(df) )
  }
  summary <- df %>%
    dplyr::mutate_if ( is.numeric, funs(weights*.) )%>%
    dplyr::group_by ( country_code,  region_nuts_codes ) %>%
    dplyr::summarize_if ( is.numeric, mean, na.rm =TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate ( country_code = as.character(country_code)) %>%
    dplyr::mutate ( region_nuts_codes = tolower(
      as.character( region_nuts_codes)))

  if (nuts_code == "code2010" ) {
    imputed <- eurobarometer::nuts2_imputation %>%
      dplyr::filter ( country_code %in% impute_countries ) %>%
      dplyr::select ( country_code, code2010, region_nuts_codes)     %>%
      dplyr::mutate ( region_nuts_codes = tolower(as.character(region_nuts_codes))) %>%
      dplyr::left_join ( summary, by = c("country_code", "region_nuts_codes") ) %>%
      dplyr::select ( -country_code )
  }

  if (nuts_code == "code2013" ) {
    imputed <- eurobarometer::nuts2_imputation %>%
      dplyr::filter ( country_code %in% impute_countries ) %>%
      dplyr::select ( country_code, code2013, region_nuts_codes) %>%
      dplyr::mutate ( region_nuts_codes = tolower(as.character(region_nuts_codes))) %>%
      dplyr::left_join ( summary, by = c("country_code", "region_nuts_codes") ) %>%
      dplyr::select ( -country_code )
  }

  imputed
}
