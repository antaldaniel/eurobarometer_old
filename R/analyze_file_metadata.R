#' Analyze the metadata of a file
#'
#' Tries to analye the metadata of data frame read by \code{\link{zacat_id_data}}.
#' @param read_df A data frame read in by \code{\link{zacat_id_data}}
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
#' analyze_file_metadata(
#'      metadata = NULL,
#'      read_df = read_df,
#'      see_log = see_log,
#'      create_log = creat_log,
#'      my_treshold = my_treshold )
#' }
#' @export


analyze_file_metadata <- function (
  read_df,
  metadata_dir = NULL,
  save_file = TRUE,
  see_log = TRUE,
  create_log = TRUE,
  log_prefix = NA,
  log_id = NA,
  my_treshold = futile.logger::INFO) {
  selected_file <-  NULL

  tryCatch({
      metadata_read_message <- "Trying to analyze the file.\nThis may take a few minutes."
      if (see_log)    futile.logger::flog.info(metadata_read_message)
      if (create_log) futile.logger::flog.info(metadata_read_message,
                                               name  ="info")
      metadata <- analyze_gesis_file(
        analyze_file = read_df,
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
                              gsub(metadata_dir, "", selected_file))
      saveRDS( metadata, paste0(tempdir(), "\\", metadata_backup ))
    }
    ) #end of tryCatch
  return ( metadata )
}
