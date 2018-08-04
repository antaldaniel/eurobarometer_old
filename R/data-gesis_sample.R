#' A small GESIS import sample (1)
#'
#' A small replication set of data for taken from the GESIS archive file
#' European Commission, Brussels (2018): Eurobarometer 81.5 (2014).
#' TNS opinion. GESIS Data Archive, Cologne. ZA5929
#' Data file Version 3.0.0, doi:10.4232/1.12969
#' @format A data frame with 27910 rows and 13 variables:
#' \describe{
#'   \item{uniqid}{Unique row id.}
#'   \item{gesis_archive_version_date}{Original versioning information with notice
#'   of being a demo sample.}
#'   \item{country_code_iso_3166}{ISO 3166 country codes, Germany is represented
#'   by DE-W and DE-E and the United Kingdom as GB-GBN and GB-NIR}
#'   \item{country_code}{Country codes without internal country division}
#'   \item{life_satisfaction}{Trend variable, converted as numeric}
#'   \item{expectations_life_in_general}{Trend variable, converted as numeric}
#'   \item{expectations_personal_job_situation}{Trend variable, converted as numeric}
#'   \item{future_human_impact_fighting_climate_change}{Imported as factor variable.}
#'   \item{future_human_impact_job_creation}{Imported as factor variable.}
#'   \item{future_human_impact_protecting_personal_data}{Imported as factor variable.}
#'   \item{region_nuts_level} {Level of aggregation with region coding.}
#'   \item{region_nuts_codes} {The name of the region.}
#'   \item{gender}{Original labelled variable converted to numeric, female = 1}
#'   \item{age_exact}{Exact age as numeric}
#'   \item{w1}{The w1 weighting variable from the original dataset}
#' }
#' @source \url{https://dbk.gesis.org/dbksearch/sdesc2.asp?no=5929}
"gesis_sample"
