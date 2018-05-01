#' Read a data file by ZACAT ID.
#'
#' Tries to find a data file based on ZACAT ID  on a data directory path,
#' or, if omitted, on the current \code{tempdir()}. Then it tries to read in
#' the file and return it.
#' @param zacat_id \code{"ZA4744"}
#' @param data_dir \code{"data-raw/"}
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
#' @importFrom futile.logger flog.info
#' @importFrom haven read_spss
#' @examples
#' \dontrun{
#' ##use your own file path:
#' ##The log parameters can be passed on optionally as ...
#' zacat_id_data ( zacat_id = "ZA4744",
#'                 data_dir = data_dir,
#'                 see_log = see_log,
#'                 create_log = creat_log,
#'                 my_treshold = my_treshold )
#' }
#' @export
#'
zacat_id_data <- function (
  zacat_id = "ZA4744",
  data_dir = data_dir,
  metadata_dir = NULL,
  save_file = TRUE,
  see_log = TRUE,
  create_log = TRUE,
  log_prefix = NA,
  log_id = NA,
  my_treshold = futile.logger::INFO) {
  sav_file_name <- lab_file_name <- NULL

  zacat_file_name <- zacat_file_lookup(zacat_id, data_dir,
                                     see_log = see_log,
                                     create_log = create_log,
                                     my_treshold = my_treshold
                                     )

 ##lab file are labelled versions of the SPSS file, saved as RDS in the
 ##temporary directory
 if ( stringr::str_sub(zacat_file_name, -4,-1) == ".lab") {
    lab_file_name <- stringr::str_sub(zacat_file_name, -4,-1)
    if ( lab_file_name %in% dir (tempdir())) {
      if (see_log)    futile.logger::flog.info("Reading labelled data from temp directory")
      if (create_log) futile.logger::flog.info("Reading labelled data from temp directory",
                                               name  ="info")
      read_df <- readRDS( paste0(tempdir(), "\\", lab_file_name))
    }
  }

  if ( stringr::str_sub(zacat_file_name, -4,-1) == ".sav") {
    sav_file_name <- zacat_file_name
    tryCatch({
      lab_file_name <- gsub(".sav", ".lab", sav_file_name)
      read_message <- paste0("Reading in\n", sav_file_name)
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
      read_df <- haven::read_spss(sav_file_name)
    },
    error = function(cond) {
      if (see_log)    futile.logger::flog.error(cond)
      if (create_log) futile.logger::flog.error(cond,
                                                name  ="error")
    },
    warning = function(cond) {
      if (see_log)    futile.logger::flog.info(cond)
      if (create_log) futile.logger::flog.info(cond,
                                               name  ="info")
    },
    finally = {
      read_message <- paste0("Imported\n", sav_file_name, " with ",
                             nrow(read_df), " observations in ",
                             ncol(read_df), " columns.\n")
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
      saveRDS(read_df, lab_file_name)
    })
  }
  return(read_df)
}
