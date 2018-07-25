#' Read an convert a GESIS file
#'
#' Runing the analyitcs function is very resource intensive. For this reason
#' the end result is always saved to the \code{tempdir()} which is deleted
#' automatically at the end of each session.  You can retrieve the metadata
#' from here, should you need it during an interactive session.
#' @param zacat_id \code{"ZA5688"}
#' @param data_dir \code{"data-raw/"}
#' @param metadata_dir \code{NULL}
#' @param new_names \code{NULL}
#' @param conversion \code{"numeric"}
#' @param save_file \code{TRUE}
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
#' @importFrom haven read_spss as_factor zap_labels
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_sub
#' @importFrom dplyr mutate mutate_if mutate_at filter select
#' @importFrom labelled remove_labels
#' @examples
#' \dontrun{
#' ##use your own file path:
#' analyse_gesis_file( path = " ... your own file path ... ",
#'                    see_log = TRUE,
#'                    create_log = TRUE,
#'                    my_treshold = futile.logger::INFO)
#' }
#' @export
#'
gesis_file_read <- function ( zacat_id = "ZA5688",
                              data_dir = "data-raw/",
                              metadata_dir = NULL,
                              new_names = NULL,
                              conversion = "numeric",
                              save_file = TRUE,
                              see_log = TRUE,
                              create_log = TRUE,
                              log_prefix = NA,
                              log_id = NA,
                              my_treshold = futile.logger::INFO) {

 ##Setup log file creation---
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

 read_df  <- zacat_id_data ( zacat_id, data_dir,
                              see_log = see_log,
                              create_log = create_log,
                              log_id = log_id,
                              my_treshold = my_treshold )

 metadata <- zacat_id_metadata(zacat_id,data_dir,
                                see_log = see_log,
                                create_log = create_log,
                                log_id = log_id,
                                my_treshold = my_treshold )


 ###Analyze the  metadata file----
  if ( is.null(metadata)) {
    analyze_file_metadata (
      read_df = read_df,
      see_log = see_log,
      create_log = create_log,
      log_id = log_id,
      my_treshold = my_treshold
      )
  }


 ###New names----
 if ( !is.null(new_names)) {
  warning("Optional names are not yet functional")
 }

 names(read_df) <- metadata$suggested_name

 ##Conversions---
 if ( conversion == "numeric") {
   return_df <- convert_to_numeric(df = read_df, metadata = metadata)
 }

 tmp <- return_df[,which ( grepl("country_code_iso_3166", names(return_df)))]

 ##Adding country code---
 if (length(tmp)>0) {
   tmp <- stringr::str_sub(
       as.character(tmp), 1,2 )
   tmp <- as.factor(tmp)
   return_df$country_code <- tmp
 } else {
   country_code_msg <- "Country code is not found or recognized in the data frame."
   if (see_log)    futile.logger::flog.warn(country_code_msg)
   if (create_log) futile.logger::flog.warn(country_code_msg,
                                            name  ="warning")
 }

 if ( save_file ) {
    sav_file  <- zacat_file_lookup(zacat_id, data_dir,
                                   see_log = see_log,
                                   create_log = create_log,
                                   my_treshold = my_treshold)
    rds_file <- gsub(".sav", ".rds" , sav_file)
    metadata_file <- gsub( ".rds", "_metadata.rds", rds_file)
    saveRDS(return_df, rds_file)
    saveRDS(metadata, metadata_file)
    save_msg <- paste0("\nSaved data as ", rds_file, "\nSaved metadata as ",
                       metadata_file)
    if (see_log)    futile.logger::flog.info(save_msg)
    if (create_log) futile.logger::flog.info(save_msg,
                                             name  ="info")

  }
  return(return_df)
}

