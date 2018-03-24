#' Import a GESIS SPSS file
#'
#' @param path The path to the GESIS SPSS file.
#' @param new_names Defaults to \code{NULL}.
#' @param create_metadata_file If \code{TRUE} it will create
#' an .rds file with the metadata of the GESIS file.
#' @importFrom haven read_spss
#' @examples
#' \dontrun{
#' ZA5929 <- gesis_file_read("data-raw/ZA5929_v3-0-0.sav")
#' }
#' @export

gesis_file_read <- function ( path,
                              new_names = NULL,
                              create_metadata_file = TRUE ) {

  read_df <- haven::read_spss(path)

    if ( !is.null(new_names) ) {

    if ( new_names[1] == "metadata") {
      metadata <- analyze_gesis_file(read_df)
      names(read_df) <- metadata$suggested_name
      metadata_file_name <- gsub(".sav", "_metadata.rds", path)

      if (create_metadata_file == TRUE) {
        saveRDS(metadata, metadata_file_name)
        message ( "Metadata saved as\n",
                  metadata_file_name)
      }

    } else {
      if (length(new_names) == length(names(read_df))) {
        names(read_df) <- new_names
      } else {
        warning("Could not change the names, the number of GESIS names
                is different from the number of given new names.")
      }
    }
  }
  return(read_df)
}
