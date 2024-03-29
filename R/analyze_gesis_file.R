#' Analyze a GESIS file
#'
#' There is a rather cumbersome exception handling with not-unique
#' GESIS variable names, which occur when certain questions are only asked
#' in some geographical units, such as the Turkish community of Cyprus.
#' In these cases, GESIS may or may not give unique variable names. Some
#' futher problems may arrise, currently the code detects this problem.
#'
#' Runing the analyitcs function is very resource intensive. For this reason
#' the end result is always saved to the \code{tempdir()} which is deleted
#' automatically at the end of each session.  You can retrieve the metadata
#' from here, should you need it during an interactive session.
#' @param analyze_file The full path to the GESIS SPSS file or a data.frame
#' read by haven consisting the contents of the file.
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
#' @importFrom haven read_spss as_factor
#' @importFrom stringr str_split str_trim
#' @importFrom utils write.csv
#' @importFrom magrittr '%>%'
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr mutate add_count distinct group_by select
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

analyze_gesis_file <- function ( analyze_file,
                                 see_log = TRUE,
                                 create_log = TRUE,
                                 log_prefix = NA,
                                 log_id = NA,
                                 my_treshold = futile.logger::INFO ) {

  df <- gesis_name <- n <- spss_name <- na_count <- . <- NULL
  suggested_name <- suggested_conversion <- suggested_conversion  <- NULL
  insert_file_name <- emergency_name <- value_labels <- NULL
  questionnaire_item <- spss_class <- NULL
  date_of_interview <- NULL
  treshold <- futile.logger::flog.threshold(my_treshold)
  directory_message <- NA

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

  if ( sum(c("character", "data.frame") %in% class(analyze_file)) < 1) {
    stop ("A file name in a character string
          or a data.frame (tibble) must be used as analyze_file parameter")
  }
  ##If a file name is inputed
  if ( "character" %in% class(analyze_file) ) {
    insert_file_name <- analyze_file
    tryCatch({
      read_df <- haven::read_spss(analyze_file)
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
      read_message <- paste0("Reading in\n", analyze_file, "\nwith ",
                             nrow(read_df), " observations in ",
                             ncol(read_df), " columns.\n")
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
    }) #end of tryCatch
  } #end of if

  ##If gesis_file is an already existing data.frame or tibble.
  if ( "data.frame" %in% class(analyze_file) ) {
    tryCatch({
      read_df <- analyze_file
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
      read_message <- paste0("Reading in pre-existing data frame with ",
                             nrow(read_df), " observations in ",
                             ncol(read_df), " columns.\n")
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message,
                                               name  ="info")
    }) #end of tryCatch
    } #end of if

  spss_metadata <- data.frame (
    gesis_name = vector (mode = "character", length = ncol(read_df)),
    spss_name = vector (mode = "character", length = ncol(read_df)),
    suggested_name = vector (mode = "character", length = ncol(read_df)),
    spss_class = vector (mode = "character", length = ncol(read_df)),
    suggested_conversion = vector (mode = "character", length = ncol(read_df)),
    value_labels = vector (mode = "character", length = ncol(read_df)),
    questionnaire_item = vector (mode = "character", length = ncol(read_df)),
    stringsAsFactors = FALSE
  )

  return_metadata <- spss_metadata

  if (see_log)    futile.logger::flog.info("Getting SPSS labels")
  if (create_log) futile.logger::flog.info("Getting SPSS labels", name  ="info")
  spss_metadata$gesis_name <- sjlabelled::get_label (read_df)

  if (see_log)    futile.logger::flog.info("Getting SPSS names")
  if (create_log) futile.logger::flog.info("Getting SPSS names", name  ="info")
  spss_metadata$spss_name <- names (read_df)

  if (see_log)    futile.logger::flog.info("Getting SPSS classes")
  if (create_log) futile.logger::flog.info("Getting SPSS classes", name  ="info")
  spss_metadata$spss_class <- vapply(read_df, class, character(1))

  if (see_log)    futile.logger::flog.info("Identifying questionnaire IDs in the long GESIS label")
  if (create_log) futile.logger::flog.info("Identifying questionnaire IDs in the long GESIS label", name  ="info")
  spss_metadata$questionnaire_item <- vapply(spss_metadata$gesis_name,
                                             questionnaire_item_identify,
                                             character(1))

  spss_metadata$suggested_name <- ifelse (
    stringr::word(tolower(spss_metadata$gesis_name),
                           1,1, " ") == spss_metadata$questionnaire_item,
           yes = stringr::word(tolower(spss_metadata$gesis_name), 2,-1, " "),
            no = tolower(spss_metadata$gesis_name))

  spss_metadata$suggested_name <- ifelse ( spss_metadata$questionnaire_item == "w1",
                                           yes = "w1",
                                           no = spss_metadata$suggested_name )

  spss_metadata$suggested_name <- ifelse ( is.na(spss_metadata$suggested_name),
           yes = tolower(spss_metadata$gesis_name),
           no = spss_metadata$suggested_name )

  if (see_log)    futile.logger::flog.info("Getting unique labels")
  if (create_log) futile.logger::flog.info("Getting unique labels", name  ="info")
  spss_metadata$value_labels <- vapply ( read_df,
                                         unique_value_labels,
                                         character(1) )

  if (see_log)    futile.logger::flog.info("Suggesting conversions.")
  if (create_log) futile.logger::flog.info("Suggesting conversions.",
                                           name  ="info")

  spss_metadata$suggested_conversion <- vapply ( read_df,
                                                 class_conversion_suggest,
                                                 character(1) )


  #explicit exceptions for keep_numeric_vars
  keep_numeric_vars <- c("duration_of_interview_minutes",
                         "level_in_society_self_placement",
                         "n_of_persons_present_during_interview")

  spss_metadata$suggested_conversion  <- ifelse (
    spss_metadata$suggested_name %in% keep_numeric_vars,
    yes = "keep_numeric",
    no = spss_metadata$suggested_conversion
  )

  spss_metadata$suggested_conversion  <- ifelse ( is.na(
    spss_metadata$suggested_conversion ),
    yes = "character",
    no = spss_metadata$suggested_conversion
  )

  suggest_message <- paste0("Creating the suggested variables names.")
  if (see_log)    futile.logger::flog.info(suggest_message)
  if (create_log) futile.logger::flog.info(suggest_message,
                                           name  ="info")

  spss_metadata$suggested_name <- gesis_longname_harmonization(
    spss_metadata$gesis_name
  )

  ##Exceptions based on short spss name
  naming_exc_message <- paste0("Reviewing exceptions with get_naming_exceptions()")
  if (see_log)    futile.logger::flog.info( naming_exc_message )
  if (create_log) futile.logger::flog.info( naming_exc_message , name  ="info")

  spss_metadata$suggested_name <- gesis_shortname_harmonization(
    short_name = spss_metadata$spss_name,
    long_name  = spss_metadata$suggested_name
  )

  spss_metadata <- spss_metadata %>%
    dplyr::mutate ( suggested_name = ifelse (
      suggested_conversion == "multiple_choice",
      yes = paste0("mc_", suggested_name ),
      no = suggested_name))

  spss_metadata <- spss_metadata %>%
    dplyr::mutate (suggested_conversion  = ifelse ( stringr::str_sub(spss_metadata$suggested_name,1,7) == "region_",
                                                    yes = "european_regions",
                                                    no = suggested_conversion )) %>%
    dplyr::mutate (suggested_conversion  = ifelse ( stringr::str_sub(spss_metadata$suggested_name,1,18) == "size_of_community_",
                                                    yes = "size_of_community",
                                                    no = suggested_conversion )) %>%
    dplyr::mutate ( suggested_conversion  = ifelse ( suggested_name == "left_right_placement",
                                                     yes = "keep_numeric",
                                                     no = suggested_conversion )) %>%
    dplyr::mutate (suggested_conversion  = ifelse ( spss_metadata$suggested_name == "date_of_interview",
                                                    yes = "rescale_date_interview",
                                                    no = suggested_conversion )) %>%
    dplyr::mutate ( suggested_name = ifelse ( stringr::str_sub(gesis_name, 1,7) ==
                                                "SPLIT (",
                                              yes = "split",
                                              no = suggested_name)) %>%
    dplyr::mutate ( suggested_name = stringi::stri_trans_general(suggested_name, "latin-ascii"))

  count_answers <- function(x) {
    if (x == "") return ( as.numeric(NA) )
    df_sub <- read_df[, which(names(read_df) == x) ]
    as.numeric(sum (! is.na(df_sub )))
  } #function to count missings

  spss_metadata <- spss_metadata %>%
    dplyr::add_count ( gesis_name ) %>%
    dplyr::mutate ( na_count = ifelse (
      n > 1,
      yes = spss_name,
      no =  "" ))  #mark problematic rows

  tryCatch(
    { #Case 1 - no multiple names
      #Case 2 - multiple names with small smaple (_tcc)
      #Case 3 - multiple names for other reasons
      naming_exc_2 <- paste0("Analyzing naming conflicts.")
      if (see_log)    futile.logger::flog.info( naming_exc_2 )
      if (create_log) futile.logger::flog.info( naming_exc_2, name  ="info")

      if ( any (spss_metadata$n > 1 ) ) {  #Not Case 1
        spss_metadata_exc <- spss_metadata #start of exception handling
        if (see_log)    futile.logger::flog.info("Start of exception handling")

        exc_message <- "There are not unique names in the GESIS file."
        if (see_log)    futile.logger::flog.warn("There are not unique names in the GESIS file.")
        if (create_log) futile.logger::flog.warn("There are not unique names in the GESIS file.",
                                                 name  ="warning")

        if ( "split" %in% spss_metadata$suggested_name) { #if there is a split
          split_message <- "There is a split in the questionnaire."
          if (see_log)    futile.logger::flog.warn(split_message)
          if (create_log) futile.logger::flog.warn(split_message,
                                                   name  ="warning")
        } #end of split warning

        spss_metadata_exc$na_count <- vapply(
            spss_metadata$na_count,
            count_answers, numeric(1))  ##to determine additional questionnaire sample size

         #First try to identify small (tcc) sample
        spss_metadata_exc <- spss_metadata_exc %>%
          dplyr::mutate ( na_count = ifelse(is.na(na_count),
                                     max(na_count, na.rm=TRUE),
                                     na_count)) %>%
          dplyr::mutate ( emergency_name = paste0(suggested_name,"_",
                                          spss_metadata_exc$spss_name)) %>%
          dplyr::mutate ( suggested_name = ifelse ( na_count < 1200,
                                               paste0(suggested_name, "_tcc"),
                                               suggested_name)) %>%
          dplyr::select(-n) %>%
          dplyr::add_count(suggested_name)

        if (see_log)    futile.logger::flog.info("Analyzed presence of small subsample")

        if ( any (spss_metadata_exc$n > 1)) {
          #Start of case 3
          exc2_message <- "The not unique names are likely not caused by
                                 a small subsample."
          if (see_log)    futile.logger::flog.warn(exc2_message)
          if (create_log) futile.logger::flog.warn(exc2_message,
                                                   name  ="warning")

          spss_metadata_exc <- spss_metadata_exc %>%
            dplyr::mutate ( suggested_name = ifelse ( n > 1,
                                                 yes = emergency_name,
                                                 no  = suggested_name ))

          return_metadata <- spss_metadata_exc #update spss_metadata for return
          #end of Case 3
        } else {
        small_sample_message <- paste0("Not unqiue variable, small sample.")
        if (see_log)    futile.logger::flog.info(small_sample_message)
        if (create_log) futile.logger::flog.info(small_sample_message,
                                                 name = "info")
        return_metadata <- spss_metadata_exc
          } #end of Case 2
      } else {   #end of Not Case 1

      no_naming_error_message <- paste0("Not unqiue variable description.")
      if (see_log)    futile.logger::flog.info(no_naming_error_message)
      if (create_log) futile.logger::flog.info(no_naming_error_message,
                                                name  ="info")
      return_metadata <- spss_metadata
      #End of Case 1
      }

      if (! "date_of_interview" %in% return_metadata$suggested_name) {
        if (see_log) futile.logger::flog.warn ("Missing date of interview")
        futile.logger::flog.warn ( "Missing date of interview",
                                   name="warning")
      }

      return_metadata <- return_metadata %>%
        dplyr::select ( gesis_name,
                        spss_name, suggested_name,
                        suggested_conversion, value_labels,
                        questionnaire_item, spss_class)

      summary_data <- return_metadata %>%
        select ( suggested_conversion  ) %>%
        add_count( suggested_conversion  ) %>%
        group_by ( suggested_conversion ) %>%
        distinct ( suggested_conversion , n ) %>%
        as.data.frame(.)

      n_factors <- summary_data$n[which(summary_data$suggested_conversion  == "factor")]
      conversion_rate_message <- paste0("\n",
        (100-(round(n_factors / nrow(return_metadata),2)*100)), "% of the variables are detected and can be converted automatically.\n",
        (round(n_factors / nrow(return_metadata),2)*100), "% of the variables will be converted to factors without harmonizing their values."
      )
      summary_message <- paste0("\nSuggested conversion ", summary_data[1,1], ": ",
                                summary_data[1,2], "\n")

      for ( i in 2:nrow(summary_data)) {
        summary_message <- paste0(summary_message,
                                  "Suggested conversion ", summary_data[i,1], ": ",
                                  summary_data[i,2], "\n")
      }
      summary_message <- paste0(summary_message,
                                "Factors need individual attention.\n",
                                "Numeric variables can be imported to R without any problem.")

      if (see_log) futile.logger::flog.info (summary_message)
      futile.logger::flog.info ( summary_message,
                                 name="info")

      if (see_log) futile.logger::flog.info (conversion_rate_message)
      futile.logger::flog.info ( conversion_rate_message,
                                 name="info")

      metadata_name <- gsub(".sav", "_metadata.rds", gesis_name)
      if ( length(metadata_name) == 0) metadata_name <- "last_data_frame_metadata.rds"
      temporary_file_name <- paste0(tempdir(),  "\\", metadata_name)
      if (see_log) futile.logger::flog.info (paste("Saved metadata to ", temporary_file_name))
      futile.logger::flog.info ( paste("Saved metadata to ", temporary_file_name),
                                 name="info")
      saveRDS(return_metadata, temporary_file_name)

    },   #end of TryCatch
    error=function(cond) {
      if (see_log) futile.logger::flog.info ("Creating error message")
      gesis_analysis_error <- paste0("Not successful -> analyze_gesis_file, ", cond)
      #warning(gesis_analysis_error )
      futile.logger::flog.error(gesis_analysis_error, name="error")
      futile.logger::flog.info(gesis_analysis_error, name="info")
      return(return_metadata <- data.frame (
        gesis_name = "failed to read",
        spss_name = "failed to read",
        suggested_name = NA,
        spss_class = NA,
        suggested_conversion = NA,
        value_labels = NA,
        questionnaire_item = NA,
        stringsAsFactors = FALSE
      ))
    },
    warning=function(cond) {
      if (see_log) futile.logger::flog.info ("Creating warning message")
      gesis_analysis_warning <- paste0("\nFinished with the analysis of the file\n
                                       with warning\n", cond)

      futile.logger::flog.warn(gesis_analysis_warning, name="warning")
      futile.logger::flog.info(gesis_analysis_warning, name="info")
      return (return_metadata)
    },
    finally = {
      finished_message <- paste0("\nFinished with the analysis of the file without warning\n",
                                 insert_file_name,
                                 "\n with ", nrow(read_df),
                                 " observations in ",
                                 ncol(read_df), " variables.")
      return (return_metadata)

    }
  )
  if (see_log) futile.logger::flog.info (finished_message)
  futile.logger::flog.info ( finished_message,
                             name="info")
  return (return_metadata)
}
