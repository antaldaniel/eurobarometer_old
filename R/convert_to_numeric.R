#' Type conversion on a labelled data frame
#'
#' @param df A labelled data.frame to be converted to numeric
#' @param metadata the metadata
#' @param conversion_type Defaults to  \code{"factor_binary"}
#' @param see_log  \code{TRUE} which will print messages to the screen.
#' @param create_log  It will create log files in the sr_logs director.
#' @param my_treshold  Can be \code{futile.logger::WARN},
#' \code{futile.logger::INFO}, \code{futile.logger::ERROR}.Defaults to
#' \code{futile.logger::INFO}.
#' @param log_prefix Defaults to \code{NA}, in which case a
#' new one will be assigned to the logs (if they are requested.) The
#' log_prefix can be directly assigned.
#' @param log_id Defaults to \code{NA}, in which case a
#' new one will be assigned to the logs (if they are requested.) The
#' log_prefix can be directly assigned.
#' @importFrom futile.logger flog.threshold flog.appender flog.remove
#' @importFrom futile.logger flog.info flog.warn flog.error WARN INFO
#' @importFrom futile.logger appender.file
#' @importFrom stringr str_split
#' @importFrom magrittr '%>%'
#' @importFrom dplyr filter select
#' @importFrom labelled to_character
#' @examples
#' \dontrun{
#' conversion_function( df = read_df2, metadata, see_log = see_log,
#' create_log = create_log,
#' log_prefix = log_prefix,
#' log_id = log_id,
#' my_treshold = my_treshold)
#' }
#' @export
#df <- read_df2
#metadata = metadata


convert_to_numeric <- function ( df, metadata,
                                  conversion_type = "factor_binary",
                                  see_log = TRUE,
                                  create_log = TRUE,
                                  log_prefix = NA,
                                  log_id = NA,
                                  my_treshold = futile.logger::INFO) {
 spss_class <- suggested_name <- suggested_conversion <- . <- NULL
   ##Setup log file creation
  if (create_log == TRUE) {
    directory_message <- NA
    if (! file.exists("sr_logs")) {
      dir.create(file.path(paste0(getwd(), "/sr_logs")))
      directory_message <- paste("Created:\n", getwd(), "/sr_logs")

    }
    if (is.na(log_prefix)) {
      log_prefix <- paste0("sr_logs/", format(Sys.time(),
                                              "%Y_%b_%d_ %H_%M"))
    }
    if (is.na(log_id)) {
      log_id <- "_gesis_analysis_"
    }
    info <- futile.logger::flog.appender(
      futile.logger::appender.file(paste0(log_prefix,log_id, "info.log")), name="info")
    warning <- futile.logger::flog.appender(
      futile.logger::appender.file(paste0(log_prefix,log_id, "warning.log")), name="warning")
    error <- futile.logger::flog.appender(
      futile.logger::appender.file(paste0(log_prefix,log_id, "error.log")), name="error")

    if (!is.na(directory_message)) {
      futile.logger::flog.info ( directory_message,
                                 name="info")
      futile.logger::flog.info ( directory_message)
    }

  }

  factor_conversion <- metadata %>%
    dplyr::filter ( suggested_conversion == conversion_type) %>%
    dplyr::filter ( spss_class == "labelled") %>%
    dplyr::select ( suggested_name ) %>% unlist (.) %>%
    as.character(.)

  conversion_vars <- which (names (df) %in%  factor_conversion)
  if ( length(conversion_vars) == 0) stop("No conversion")
  for ( i in conversion_vars ) {
    tryCatch({
      if ( all(is.na(df[,i]))) next
      if ( conversion_type == "factor_binary") {
        tmp <- as_factor_binary(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }
      if ( conversion_type == "multiple_choice") {
        tmp <- as_factor_binary(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }
      if ( conversion_type == "factor_3") {
        tmp <- as_factor_3(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
        }
      if ( conversion_type == "factor_4") {
        tmp <- as_factor_4(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }
      if ( conversion_type == "factor_5") {
        tmp <- as_factor_5(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }
      if ( conversion_type == "factor_pos_neg") {
        tmp <- as_factor_pos_neg(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }
      if ( conversion_type == "factor_yes_no_4") {
        tmp <- as_factor_yes_no_4(x  = labelled::to_character(
          df[[i]], levels = "labels"))
        tmp <- as_numeric(tmp)
      }

      if ( conversion_type == "keep_numeric") {
        tmp <- keep_numeric(column  = labelled::to_character(
          df[[i]], levels = "labels"))
      }
      if ( conversion_type == "rescale_date_interview") {
        tmp <- rescale_date_interview(column  = labelled::to_character(
          df[[i]], levels = "labels"))
      }
    },
    error = function(cond) {
      error_message <- paste0(i, cond)
      if (see_log)    futile.logger::flog.error(error_message)
      if (create_log) futile.logger::flog.error(error_message,
                                                name  ="error")
    },
    warning = function(cond){
      warning_message <- paste0(i, cond)
      if (see_log)    futile.logger::flog.error(warning_message)
      if (create_log) futile.logger::flog.error(warning_message,
                                                name  ="error")
    },
    finally = {
      df[,i] <- tmp
    })
  }

  msg <-  paste0("\nThe ", conversion_type, " ->  numeric variable conversions took place\n",
                 paste ( factor_conversion, collapse = "\n") )
  if (see_log)    futile.logger::flog.info(msg)
  if (create_log) futile.logger::flog.info(msg,
                                           name  ="info")
  return(df)
}
