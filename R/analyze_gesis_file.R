#' Analyze a GESIS file
#'
#' There is a rather cumbersome exception handling with not-unique
#' GESIS variable names, which occur when certain questions are only asked
#' in some geographical units, such as the Turkish community of Cyprus.
#' In these cases, GESIS may or may not give unique variable names. Some
#' futher problems may arrise, currently the code detects this problem.
#' @param gesis_file The full path to the GESIS SPSS file or a data.frame
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
#' @importFrom stringr str_split
#' @importFrom utils write.csv
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate add_count
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

gesis_file <- "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/data-raw/gesis/ZA4530_v2-1-0.sav"
see_log <- create_log <- TRUE
log_prefix <- log_id <-NA
my_treshold = futile.logger::INFO
trial <- analyze_gesis_file (gesis_file)
analyze_gesis_file <- function ( gesis_file,
                                 see_log = TRUE,
                                 create_log = TRUE,
                                 log_prefix = NA,
                                 log_id = NA,
                                 my_treshold = futile.logger::INFO ) {

  df <- gesis_name <- n <- spss_name <- na_count <- . <- NULL
  suggested_name <- suggested_conversion <- NULL
  insert_file_name <- emergency_name <- NULL
  treshold <- futile.logger::flog.threshold(my_treshold)
  directory_message <- NA

  if (create_log == TRUE) {
    directory_mssage <- NA
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

  tryCatch(
    {

      if ( "character" %in% class(gesis_file) ) {
        insert_file_name <- gesis_file
        read_df <- haven::read_spss(gesis_file)
        read_message <- paste0("Reading ", gesis_file)
        if (see_log)    futile.logger::flog.info(read_message)
        if (create_log) futile.logger::flog.info(read_message,
                                                 name  ="info")
      } else if ( ! "data.frame"  %in% class(gesis_file)) {
          stop("Parameter gesis_file must be a pre-imported file or a full path.")
      } else  {
        read_df <- gesis_file
        df_message <- paste0("Inputed a pre-existing data.frame with ",
                             ncol( read_df), " variables.")
          if (see_log)    futile.logger::flog.info(df_message)
          if (create_log) futile.logger::flog.info(df_message, name  ="info")
        }

      read_message <- paste0("Analyzing the GESIS file.")
      if (see_log)    futile.logger::flog.info(read_message)
      if (create_log) futile.logger::flog.info(read_message, name  ="info")

      spss_metadata <- data.frame (
        gesis_name = vector (mode = "character", length = ncol(read_df)),
        spss_name = vector (mode = "character", length = ncol(read_df)),
        suggested_name = vector (mode = "character", length = ncol(read_df)),
        suggested_conversion = vector (mode = "character", length = ncol(read_df)),
        value_labels = vector (mode = "character", length = ncol(read_df)),
        questionnaire_item = vector (mode = "character", length = ncol(read_df)),
        spss_class = vector (mode = "character", length = ncol(read_df)),
        suggested_class = vector (mode = "character", length = ncol(read_df)),
         stringsAsFactors = FALSE
      )

      if (see_log)    futile.logger::flog.info("Getting SPSS labels")
      if (create_log) futile.logger::flog.info("Getting SPSS labels", name  ="info")
      spss_metadata$gesis_name <- sjlabelled::get_label (read_df)

      if (see_log)    futile.logger::flog.info("Getting SPSS names")
      if (create_log) futile.logger::flog.info("Getting SPSS names", name  ="info")
      spss_metadata$spss_name <- names (read_df)

      if (see_log)    futile.logger::flog.info("Getting SPSS classes")
      if (create_log) futile.logger::flog.info("Getting SPSS classes", name  ="info")
      spss_metadata$spss_class <- vapply(read_df, class, character(1))

      if (see_log)    futile.logger::flog.info("Getting unique labels")
      if (create_log) futile.logger::flog.info("Getting unique labels", name  ="info")
      spss_metadata$value_labels <- vapply ( read_df, unique_value_labels, character(1) )

      if (see_log)    futile.logger::flog.info("Suggesting conversions.")
      if (create_log) futile.logger::flog.info("Suggesting conversions.",
                                               name  ="info")
      spss_metadata$suggested_conversion <- vapply ( read_df,
                                                class_conversion_suggest,
                                                character(1) )
      spss_metadata$suggested_class <- ifelse (
        spss_metadata$suggested_conversion %in% c(
          "multiple_choice", "rescale_benefit_2",
          "rescale_for_against_2",
          "rescale_gender",
          "rescale_trust",
          "rescale_yes_no_2"
        ),
        yes = "as_factor_binary",
        no = NA
      )
      spss_metadata$suggested_class <- ifelse (
        spss_metadata$suggested_conversion %in% c(
          "rescale_time_frequency_3",
          "rescale_subjective_urbanization",
          "rescale_social_class_en",
          "rescale_low_strong_3",
          "rescale_difficulty",
          "recale_acceptable_3"
        ),
        yes = "as_factor_3",
        no = spss_metadata$suggested_class
      )
      spss_metadata$suggested_class <- ifelse (
        spss_metadata$suggested_conversion %in% c(
          "rescale_political_interest"
        ),
        yes = "as_factor_4",
        no = spss_metadata$suggested_class
      )
      spss_metadata$suggested_class <- ifelse (
        spss_metadata$suggested_conversion %in% c(
          "rescale_much_little_3",
          "rescale_goal_3",
          "rescale_direction",
          "rescale_better_worse_3",
          "rescale_amount_3"
        ),
        yes = "as_factor_3",
        no = spss_metadata$suggested_class
      )

      spss_metadata$suggested_class <- ifelse (
        spss_metadata$value_labels %in% c(
          "Frequently|Occasionally|Never",
          "Frequently|Never|Occasionally",
          "Occasionally|Never|Frequently",
          "Occasionally|Frequently|Never",
          "Never|Frequently|Occasionally",
          "Never|Occasionally|Frequently"
        ),
        yes = "as_time_frequency_",
        no = spss_metadata$suggested_class
      )
      spss_metadata$suggested_class <- ifelse (
        spss_metadata$suggested_conversion %in% c(
          "rescale_yes_no_4",
          "rescale_satisfaction",
          "rescale_situation",
          "rescale_optimism_4",
          "rescale_likely_4",
          "rescale_image",
          "rescale_description",
          "rescale_attachment_4",
          "rescale_agreement_4",
          "rescale_informed_4", "rescale_effective_4"
        ),
        yes = "as_factor_yes_no_4",
        no = spss_metadata$suggested_class
      )

      spss_metadata$suggested_class <- ifelse ( is.na(
        spss_metadata$suggested_class),
        yes = spss_metadata$suggested_conversion,
        no = spss_metadata$suggested_class
      )


      suggest_message <- paste0("Creating the suggested variables names.")
      if (see_log)    futile.logger::flog.info(suggest_message)
      if (create_log) futile.logger::flog.info(suggest_message,
                                               name  ="info")

      a <- tolower(as.character(spss_metadata$gesis_name))
      a <- gsub ( "%", "pct", a)
      a <- gsub ( "(spont)", "_spont", a)
      a <- gsub ( "(sum)", "_sum", a)
      a <- gsub ( "(summarized)", "_sum", a)
      a <- gsub ( "(summarised)", "_sum", a)
      a <- gsub ( "(recoded)", "_rec", a)
      a <- gsub ( "(rec)", "_rec", a)
      a <- gsub( " - ", ": ", a)
      a <- gsub( "\\s", "_", a)
      a <- gsub( ":_", ":", a)
      a <- gsub ( "/", "_", a)
      a <- gsub ( "\\(", "", a)
      a <- gsub ( ")", "", a)
      a <- gsub ( "\\+", "_", a)
      a <- gsub ( "\\&", "", a)
      a <- gsub ( "___", "_", a)
      a <- gsub ( "__", "_", a)

      spss_metadata$suggested_name = gsub ( ":", "_", a)

      naming_exc_message <- paste0("Reviewing exceptions with get_naming_exceptions()")
      if (see_log)    futile.logger::flog.info( naming_exc_message )
      if (create_log) futile.logger::flog.info( naming_exc_message , name  ="info")

      naming_exceptions <- get_naming_exceptions()  #must not be factors
      for (i in 1:length(naming_exceptions)) {
        spss_metadata$suggested_name <- ifelse (
          spss_metadata$gesis_name == naming_exceptions$exact[i],
          naming_exceptions$new_name[i],
          spss_metadata$suggested_name)
      }

      spss_metadata <- spss_metadata %>%
        dplyr::mutate ( suggested_name = ifelse (
          spss_name == "split",
          yes = "split", no = suggested_name)) %>%
        dplyr::mutate ( suggested_name = ifelse (
          spss_name == "uniqid",
          yes = "uniqid", no = suggested_name)) %>%
        dplyr::mutate ( suggested_name = ifelse (
          suggested_conversion == "multiple_choice",
          yes = paste0("mc_", suggested_name ),
          no = suggested_name)) %>%
        dplyr::mutate ( suggested_name = gsub("_10p-scale", "", suggested_name)) %>%
        dplyr::mutate ( suggested_name = gsub("aged_<10", "aged_10m", suggested_name )) %>%
        dplyr::mutate ( suggested_name = gsub("_aged_15_", "aged_15p", suggested_name )) %>%
        dplyr::mutate ( suggested_name = gsub("aged_10-14", "aged_10_14", suggested_name )) %>%
        dplyr::mutate ( suggested_name = ifelse( spss_name == "wex",
                                         yes = "wex",
                                         no = suggested_name )) %>%
        dplyr::mutate ( suggested_name = ifelse ( tolower(spss_name) == "w1",
                                           yes = "w1",
                                           no = suggested_name ))

      spss_metadata <- spss_metadata %>%
        dplyr::add_count ( gesis_name )

      if ( any (spss_metadata$n > 1 ) ) {
        if ( "split" %in% spss_metadata$suggested_name) {
          split_message <- paste0("There is a split in the questionnaire with not unique names in the GESIS file.")
          if (see_log)    futile.logger::flog.warn(split_message)
          if (create_log) futile.logger::flog.warn(split_message,
                                                   name  ="warning")
        }

        spss_metadata <- spss_metadata %>%
          dplyr::mutate ( na_count = ifelse ( n > 1,
                                       spss_name,
                                       "" ))
        count_answers <- function(x) {
            if (x == "") return ( as.numeric(NA) )
            df_sub <- read_df[, which(names(read_df) == x) ]
            as.numeric(sum (! is.na(df_sub )))
        }

        spss_metadata_exc <- spss_metadata

        spss_metadata_exc$na_count <- vapply(
            spss_metadata$na_count,
            count_answers, numeric(1))  ##to determine additional questionnaire sample size


        spss_metadata_exc <-  spss_metadata_exc %>%
          dplyr::mutate ( na_count = ifelse(is.na(na_count),
                                     max(na_count, na.rm=TRUE),
                                     na_count)) %>%
          dplyr::mutate ( emergency_name = paste0(suggested_name,"_",
                                                  row.names(spss_metadata_exc))) %>%
          dplyr::mutate ( suggested_name = ifelse ( na_count < 12000,
                                               paste0(suggested_name, row.names(.)),
                                               suggested_name )) %>%
          dplyr::select ( -na_count, -n) %>%
          dplyr::add_count ( suggested_name )

        if ( any (spss_metadata_exc$n > 1)) {
          spss_metadata_exc <-  spss_metadata_exc %>%
            dplyr::mutate ( suggested_name = ifelse ( n > 1,
                                                      emergency_name,
                                                      suggested_name ))
          unknow_naming_error_message <- paste0("Not unqiue variable description in\n", gesis_file)
          if (see_log)    futile.logger::flog.error(unknow_naming_error_message)
          if (create_log) futile.logger::flog.error(unknow_naming_error_message,
                                                   name  ="error")
          warning ( unknow_naming_error_message )
          spss_metadata <- spss_metadata_exc
        } else {
          spss_metadata <- spss_metadata_exc
        }
      }

      spss_metadata <-spss_metadata_exc %>%
        dplyr::select ( gesis_name, spss_name, suggested_name,
        suggested_conversion, value_labels,
        questionnaire_item, spss_class,
        suggested_class)

    },
    error=function(cond) {
      gesis_analysis_error <- paste0("Not successful -> analyze_gesis_file, ", cond)
      warning(gesis_analysis_error )
      futile.logger::flog.error(gesis_analysis_error, name="error")
      futile.logger::flog.info(gesis_analysis_error, name="info")
    },
    warning=function(cond) {
      gesis_analysis_warning <- paste0("Warning -> analyze_gesis_file, ", cond)
      futile.logger::flog.warn(gesis_analysis_warning, name="warning")
      futile.logger::flog.info(gesis_analysis_warning, name="info")
    },
    finally = {
      finished_message <- paste0("Finished with the analysis of the file\n",
                                 insert_file_name,
                                 "\n with ", nrow(read_df),
                                 " observations in ",
                                 ncol(read_df), " variables.")
      if (see_log) futile.logger::flog.info (finished_message)
      futile.logger::flog.info ( finished_message,
                                 name="info")
    }
  )

  return (spss_metadata)
}
