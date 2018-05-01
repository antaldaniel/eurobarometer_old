#' Read a mdata file by ZACAT ID.
#'
#' Tries to find a metadata file based on ZACAT ID  on a data directory path,
#' or, if omitted, on the current \code{tempdir()}.
#' @param zacat_id \code{"ZA4744"}
#' @param metadata_dir Defaults to \code{NULL}.
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
#' zacat_id_metadata( zacat_id = "ZA4744",
#'                 data_dir = data_dir,
#'                 see_log = see_log,
#'                 create_log = creat_log,
#'                 my_treshold = my_treshold )
#' }
#' @export

zacat_id_metadata <- function (
  zacat_id = "ZA4744",
  metadata_dir = NULL,
  see_log = TRUE,
  create_log = TRUE,
  log_prefix = NA,
  log_id = NA,
  my_treshold = futile.logger::INFO) {


  #Searching for metadata file----
  if  ( is.null(metadata_dir)) {
    metadata_dir <- tempdir()
  }
  metadata_dir_files <- dir ( metadata_dir)
  metadata_dir_files <- metadata_dir_files[grepl("_metadata.rds",
                                                 metadata_dir_files)]
  selected_metadata_file <- which (grepl(zacat_id, metadata_dir_files))

  if ( length(selected_metadata_file) == 0) {
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

      if ( is.null(metadata)) {
        null_metadata_file_msg <- "The metadata file is empty or could not be read."
        if (see_log)    futile.logger::flog.info(null_metadata_file_msg)
        if (create_log) futile.logger::flog.info(null_metadata_file_msg,
                                                 name  ="error")
      }
    }
  } #end of else

  return (metadata)
}
