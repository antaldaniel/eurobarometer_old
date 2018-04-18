#' Type conversion on a labelled data frame
#'
#' @param df A labelled data.frame to be converted to numeric
#' @param metadata the metadata
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

 return_df <- matrix ( data = NA, nrow = nrow(df), ncol = ncol(df))
 return_df <- as.data.frame(return_df)
 names (return_df) <- names (df)

 numeric_vars <- which (metadata$suggested_conversion == "numeric")
 chr_vars <- which ( ! metadata$suggested_conversion %in% c("factor_3",
                                                            "factor_4", "factor_5", "factor_binary", "factor_pos_neg",
                                                            "factor_yes_no_4", "multiple_choice", "keep_numeric",
                                                            "rescale_date_interview", "numeric"))

 paste(names (df)[convert_factor_3], collapse = ", ")
 convert_factor_3 <-  which (metadata$suggested_conversion == "factor_3")
 convert_factor_4 <-  which (metadata$suggested_conversion == "factor_4")
 convert_factor_5 <-  which (metadata$suggested_conversion == "factor_5")
 convert_factor_binary <-  which (metadata$suggested_conversion == "factor_binary")
 convert_pos_neg <-  which (metadata$suggested_conversion == "factor_pos_neg")
 convert_yes_no_4 <-  which (metadata$suggested_conversion == "factor_yes_no_4")
 convert_multiple_choice <-  which (metadata$suggested_conversion == "multiple_choice")
 convert_keep_numeric <- which (metadata$suggested_conversion == "keep_numeric")
 convert_rescale_date <- which (metadata$suggested_conversion == "rescale_date_interview")


 factor_3_message <- paste0("Converting ", length(convert_factor_3), " three level factors to numeric,\n",
                            paste(names (df)[convert_factor_3], collapse = ", ") )
 futile.logger::flog.info (factor_3_message, name="info")
 futile.logger::flog.info (factor_3_message )
 for (i in convert_factor_3) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_3(df[[i]]))
   },
  error = function(cond) {
       error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                   names(df)[i], "\n", cond)
       if (see_log)    futile.logger::flog.error(error_chr_convert)
       if (create_log) futile.logger::flog.error(error_chr_convert,
                                                 name  ="error")
       return_df[[i]] <- as.character(df[[i]])
  },
  warning = function(cond){
       warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
       if (see_log)    futile.logger::flog.warn(warning_chr_convert)
       if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                                 name  ="warn")
  },
  finally = {})
 }


 factor_4_message <- paste0("Converting ", length(convert_factor_4), " three level factors to numeric,\n",
                            paste(names (df)[convert_factor_4], collapse = ", ") )
 futile.logger::flog.info (factor_4_message, name="info")
 futile.logger::flog.info (factor_4_message )
 for (i in convert_factor_4) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_4(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 factor_5_message <- paste0("Converting ", length(convert_factor_5), " three level factors to numeric,\n",
                            paste(names (df)[convert_factor_5], collapse = ", ") )
 futile.logger::flog.info (factor_5_message, name="info")
 futile.logger::flog.info (factor_5_message )
 for (i in convert_factor_5) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_5(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 factor_binary_message <- paste0("Converting ", length(convert_factor_binary),
                            " binary factors to numeric:\n",
                            paste(names (df)[convert_factor_binary], collapse = ", ") )
 futile.logger::flog.info ( factor_binary_message, name="info")
 futile.logger::flog.info ( factor_binary_message )
 for (i in convert_factor_binary) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_binary(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }
 factor_mc_message <- paste0("Converting ", length(convert_multiple_choice),
                                 " binary multiple_choice variables to numeric:\n",
                                 paste(names (df)[convert_multiple_choice],
                                       collapse = ", ") )
 futile.logger::flog.info ( factor_mc_message, name="info")
 futile.logger::flog.info ( factor_mc_message )
 for (i in convert_multiple_choice) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_binary(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }
 factor_pos_neg_msg <- paste0("Converting ", length(convert_pos_neg),
                             " three-level (negative, neutral, positive) factors:\n",
                             paste(names (df)[convert_pos_neg],
                                   collapse = ", ") )
 futile.logger::flog.info ( factor_pos_neg_msg, name="info")
 futile.logger::flog.info ( factor_pos_neg_msg )
 for (i in convert_pos_neg) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_pos_neg(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 factor_yes_no_4_msg <- paste0("Converting ", length(convert_yes_no_4),
                              " four-level (2 negative, 2 positive) factors:\n",
                              paste(names (df)[convert_yes_no_4],
                                    collapse = ", ") )
 futile.logger::flog.info ( factor_yes_no_4_msg, name="info")
 futile.logger::flog.info ( factor_yes_no_4_msg)
 for (i in convert_yes_no_4) {
   tryCatch({
     return_df[[i]] <- as_numeric(as_factor_yes_no_4(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 convert_keep_numeric_msg <- paste0("Converting ", length(convert_keep_numeric),
                            " numeric variables with words or units:\n",
                            paste(names (df)[convert_keep_numeric],
                                  collapse = ", ") )
 futile.logger::flog.info ( convert_keep_numeric_msg, name="info")
 futile.logger::flog.info ( convert_keep_numeric_msg  )
 for (i in convert_keep_numeric) {
   tryCatch({
     return_df[[i]] <- keep_numeric(df[[i]])
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }


 convert_date_msg <- paste0("Converting ", length(convert_rescale_date),
                               " three-level (negative, neutral, positive) factors:\n",
                               paste(names (df)[convert_rescale_date],
                                     collapse = ", ") )
 futile.logger::flog.info ( convert_date_msg , name="info")
 futile.logger::flog.info ( convert_date_msg )
 for (i in convert_rescale_date) {
   tryCatch({
     return_df[[i]] <-  rescale_date_interview(df[[i]])
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 numeric_vars_msg <- paste0("Removing labels from ", length(numeric_vars),
                            " numeric variables:\n",
                            paste(names (df)[numeric_vars],
                                  collapse = ", ") )
 futile.logger::flog.info ( numeric_vars_msg  , name="info")
 futile.logger::flog.info ( numeric_vars_msg  )
 for (i in numeric_vars) {
   tryCatch({
     return_df[[i]] <-  as.numeric(labelled::to_character(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

 chr_vars_msg <- paste0("Removing labels from ", length(chr_vars),
                            " character variables:\n",
                            paste(names (df)[chr_vars],
                                  collapse = ", ") )
 futile.logger::flog.info ( chr_vars_msg, name="info")
 futile.logger::flog.info ( chr_vars_msg )
 for (i in chr_vars) {
   tryCatch({
     return_df[[i]] <-  as.factor(labelled::to_character(df[[i]]))
   },
   error = function(cond) {
     error_chr_convert <- paste0(i, "/", ncol(df), " ",
                                 names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.error(error_chr_convert)
     if (create_log) futile.logger::flog.error(error_chr_convert,
                                               name  ="error")
     return_df[[i]] <- as.character(df[[i]])
   },
   warning = function(cond){
     warning_chr_convert <- paste0(i, "/", ncol(df), " ", names(df)[i], "\n", cond)
     if (see_log)    futile.logger::flog.warn(warning_chr_convert)
     if (create_log) futile.logger::flog.warn(warning_chr_convert,
                                              name  ="warn")
   },
   finally = {})
 }

return(return_df)
}
