#' A small excerpt from a GESIS file for testing
#'
#' The sample is read from an SPSS file. The raw SPSS file has the
#' original labels. As you can see, \code{qa1a_2} and \code{qa1b_2}
#' have the same variable name in GESIS.
#' \code{"gesis_small_sample <- haven::read_spss(
#' data-raw/gesis_small_sample.sav')"}
#' \describe{
#'   \item{uniqid}{Respondent unique ID }
#'   \item{serialid}{SERIAL CASE ID (APPOINTED BY TNS)}
#'   \item{split}{QUESTIONNAIRE SPLIT (QC2A & QC2B) }
#'   \item{qa1a_2}{SITUATION: EUROPEAN ECONOMY}
#'   \item{qa1b_2}{SITUATION: EUROPEAN ECONOMY}
#'   \item{qb1_3}{EU 2020 GOALS: REDUCE GREENHOUSE GAS BY 20 PCT}
#'   \item{qb1_4}{EU 2020 GOALS: INCREASE RENEWABLE ENERGY BY 20 PCT}
#' }
#' @source \url{https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6863}
"gesis_small_sample"
