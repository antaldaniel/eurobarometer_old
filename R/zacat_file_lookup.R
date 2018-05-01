#' Look up a ZACAT file in a directory
#'
#' Tries to find a data file based on ZACAT ID  on a data directory path,
#' or, if omitted, on the current \code{tempdir()}.
#' @param zacat_id \code{NULL}
#' @param data_dir \code{NULL}
#' @param see_log  \code{TRUE} which will print messages to the screen.
#' @param create_log  It will create log files in the sr_logs director.
#' @param my_treshold  Can be \code{futile.logger::WARN},
#' \code{futile.logger::INFO}, \code{futile.logger::ERROR}.Defaults to
#' \code{futile.logger::INFO}.
#' @importFrom futile.logger flog.info
#' @importFrom haven read_spss
#' @examples
#' \dontrun{
#' ##use your own file path:
#' ##The log parameters can be passed on optionally as ...
#' zacat_file_lookup ( zacat_id = "ZA4744",
#'                 data_dir = data_dir,
#'                 see_log = see_log,
#'                 create_log = creat_log,
#'                 my_treshold = my_treshold )
#' }
#' @export
#'

zacat_file_lookup <- function (
  zacat_id = NULL,
  data_dir = NULL,
  see_log = TRUE,
  create_log = TRUE,
  my_treshold = futile.logger::INFO
 ) {
  if ( is.null(zacat_id)) stop ("No ZACAT ID is given.")

  if  ( is.null(data_dir)) {
    if (see_log)    futile.logger::flog.info("Trying to find ZACAT file in the currenty temporary directory")
    if (create_log) futile.logger::flog.info("Trying to find ZACAT file in the currenty temporary directory",
                                             name  ="info")
    data_dir <- tempdir()
  }


  data_dir_files <- dir ( data_dir )
  data_dir_files <- data_dir_files[grepl(".sav", data_dir_files)]
  selected_file  <- which (grepl(zacat_id, data_dir_files))
  if ( length(selected_file) == 0 ) stop("No such file in the data directory.")
  if ( length(selected_file) >  1 ) stop("Multiple files are matched by " ,
                                         zacat_id, "\nPlease use a unique identifier.")
  selected_file_name <- paste0(data_dir, data_dir_files [selected_file])

  return (selected_file_name)
}
