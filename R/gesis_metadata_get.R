#' Get a metadata file
#'
#' Retrieve a metadata file with from your data directory or the temporary directory
#' of your current R session.
#' @param zacat_id For example, \code{"ZA5688_v6-0-0"}, as analyzed and saved by
#' \code{\link{analyze_gesis_file}}.
#' Defaults to \code{"last_data_frame"}, which is
#' the last file in the temporary directory of the current session.
#' @param data_dir Defaults to \code{"NULL"}. In this case the \code{"tempdir()"}
#' of your current R session will be used.
#' @examples
#' \dontrun{
#' ##use your own file:
#' gesis_metadata_get( zacat_id = "ZA5688",
#'                     data_dir = "data-raw/")
#' }
#' @export
#'
gesis_metadata_get <- function ( zacat_id = "last_data_frame",
                                 data_dir = NULL ) {

  if (is.null(zacat_id)) stop("No ZACAT ID is given")
  if (is.null(data_dir)) data_dir = tempdir()
  related_files <- dir(data_dir)[which ( grepl(zacat_id, dir(data_dir)))]
  metadata_file <- related_files [grepl("_metadata", related_files)]
  metadata_path <- paste0(data_dir,"\\" , metadata_file)

  tryCatch({
      if ( !is.null(metadata_file)) {
        return(readRDS(metadata_path))
      } else {
        stop("File not found in the data (temporary) directory.")
      }
  },
  error = function(cond) {
    stop("Failed to read metadata file, ", cond)
  },
  warning = function(cond) {
    warning(cond)
  },
  finally = {}
  )
}


