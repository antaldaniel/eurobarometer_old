#' Read an convert a GESIS file
#'
#' Runing the analyitcs function is very resource intensive. For this reason
#' the end result is always saved to the \code{tempdir()} which is deleted
#' automatically at the end of each session.  You can retrieve the metadata
#' from here, should you need it during an interactive session.
#' @param zacat_id \code{"ZA4744"}
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
gesis_file_read <- function ( zacat_id = "ZA4744",
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

  ##Reading the data file----
  if  ( is.null(data_dir )) data_dir <- tempdir()
  data_dir_files  <- dir ( data_dir )
  data_dir_files <- data_dir_files[grepl(".sav", data_dir_files)]
  selected_file <- which (grepl(zacat_id, data_dir_files))
  if ( length(selected_file) == 0 ) stop("No such file in the data directory.")
  if ( length(selected_file) >  1 ) stop("Multiple files are matched by " ,
                                         zacat_id, "\nPlease use a unique identifier.")
  selected_file <- paste0(data_dir, data_dir_files [selected_file])

  #Searching for metadata file----
  if  ( is.null(metadata_dir ) ) metadata_dir <- tempdir()
  metadata_dir_files <- dir ( metadata_dir)
  metadata_dir_files <- metadata_dir_files[grepl("_metadata.rds",
                                         metadata_dir_files)]
  selected_metadata_file <- which (grepl(zacat_id, metadata_dir_files))


  if ( length(selected_metadata_file ) == 0) {
    metadata <- NULL
  } else {
    if ( length(selected_metadata_file) >  1 ) stop("Multiple files are matched by " ,
                                           zacat_id, "\nPlease use a unique identifier.")
    if ( length(selected_metadata_file) == 1 ) {
      selected_metadata_file <- paste0(metadata_dir, "\\",
                                       metadata_dir_files [  selected_metadata_file])
      if (see_log)    futile.logger::flog.info("Reading metadata from temp directory")
      if (create_log) futile.logger::flog.info("Reading metadata data from temp directory",
                                               name  ="info")
      metadata <- readRDS(selected_metadata_file)
    }

  }


  ##Setup log file creation ---

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

  ##Try to read in the file
  lab_file_name <- gsub(".sav", ".lab",
                        gsub(data_dir, "", selected_file ))


  if ( lab_file_name %in% dir (tempdir())) {
    if (see_log)    futile.logger::flog.info("Reading labelled data from temp directory")
    if (create_log) futile.logger::flog.info("Reading labelled data from temp directory",
                                             name  ="info")
    read_df <- readRDS( paste0(tempdir(), "\\", lab_file_name))
  } else {
    tryCatch({
      read_message <- paste0("Reading in\n", selected_file)
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
      read_df <- haven::read_spss(selected_file)
    },
    error = function(cond) {
      if (see_log)    futile.logger::flog.error(cond)
      if (create_log) futile.logger::flog.error(cond,
                                                name  ="error")
      stop("Failed to read in the file.")
    },
    warning = function(cond) {
      if (see_log)    futile.logger::flog.info(cond)
      if (create_log) futile.logger::flog.info(cond,
                                               name  ="info")
    },
    finally = {
      read_message <- paste0("Imported\n", selected_file, "\nwith ",
                             nrow(read_df), " observations in ",
                             ncol(read_df), " columns.\n")
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
      saveRDS( read_df, paste0(tempdir(), "\\", lab_file_name))
    }
    )
    }

 if ( is.null(metadata) ) {
   tryCatch({
     metadata_read_message <- "Trying to analyze the file.\nThis may take a few minutes."
     if (see_log)    futile.logger::flog.info(metadata_read_message)
     if (create_log) futile.logger::flog.info(metadata_read_message,
                                              name  ="info")
     metadata <- analyze_gesis_file( selected_file ,
                    see_log = see_log,
                    create_log = create_log,
                    log_prefix = log_prefix,
                    log_id = log_id,
                    my_treshold = my_treshold )

     if ( all(metadata$gesis_name == "")) { stop("Wrong analysis.")}
   },
   error = function(cond) {
     if (see_log)    futile.logger::flog.error(cond)
     if (create_log) futile.logger::flog.error(cond,
                                               name  ="error")
     stop("Failed to analyze the file.")
   },
   warning = function(cond) {
     if (see_log)    futile.logger::flog.info(cond)
     if (create_log) futile.logger::flog.info(cond,
                                              name  ="info")
   },
   finally = {
     metadata_message <- "Finished with the analyzis."
     if (see_log)    futile.logger::flog.info(metadata_message)
     if (create_log) futile.logger::flog.info(metadata_message,
                                              name  ="info")
     metadata_backup <- gsub(".sav", "_metadata.rds",
                             gsub(data_dir, "", selected_file))
     saveRDS( metadata, paste0(tempdir(), "\\", metadata_backup ))
   }
   )}


 if ( !is.null(new_names)) {
  warning("Optional names are not yet functional")
 }

 names(read_df) <- metadata$suggested_name

 if ( "country_code_iso_3166" %in% names (read_df)) {
   read_df <- read_df %>%
     dplyr::mutate (country_code = as.factor(
       stringr::str_sub(
         read_df$country_code_iso_3166, 1,2 )
     ) )
 } else {
   country_code_msg <- "Country code is not found or recognized in the data frame."
   if (see_log)    futile.logger::flog.warn(country_code_msg)
   if (create_log) futile.logger::flog.warn(country_code_msg,
                                            name  ="warning")

 }

 conversion_types <- c("factor_binary", "multiple_choice",
                       "factor_3", "factor_4", "factor_5",
                       "factor_pos_neg", "factor_yes_no_4",
                       "keep_numeric", "rescale_date_interview")

 for (i in 1:length(conversion_types)) {
   tryCatch(
     {
       read_df <- convert_to_numeric( df = read_df,
                                     metadata = metadata,
                                     conversion_type = conversion_types[i],
                                     see_log = see_log,
                                     create_log = create_log,
                                     log_prefix = log_prefix,
                                     log_id = log_id,
                                     my_treshold = my_treshold)
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
       info_msg <- paste0(conversion_types[i], " finished.\n")
       if (see_log)    futile.logger::flog.info(info_msg)
       if (create_log) futile.logger::flog.info(info_msg,
                                                 name  ="info")
     }
    )

   }
  read_df <- read_df %>%
    dplyr::mutate_if ( haven::is.labelled, haven::as_factor) %>%
    dplyr::mutate_all ( haven::zap_labels)

  if ( save_file ) {
    rds_file <- gsub(".sav", ".rds" , selected_file)
    metadata_file <- gsub( ".rds", "_metadata.rds", rds_file)
    saveRDS(read_df, rds_file)
    saveRDS(metadata, metadata_file)
    save_msg <- paste0("Saved as\n", rds_file, "\nand\n",
                       metadata_file)
    if (see_log)    futile.logger::flog.info(save_msg)
    if (create_log) futile.logger::flog.info(save_msg,
                                             name  ="info")

  }
  return(read_df)
}

