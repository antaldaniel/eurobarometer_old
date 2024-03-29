% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zacat_id_data.R
\name{zacat_id_data}
\alias{zacat_id_data}
\title{Read a data file by ZACAT ID.}
\usage{
zacat_id_data(zacat_id = "ZA4744", metadata_dir = NULL,
  save_file = TRUE, see_log = TRUE, create_log = TRUE,
  log_prefix = NA, log_id = NA, my_treshold = futile.logger::INFO)
}
\arguments{
\item{zacat_id}{\code{"ZA4744"}}

\item{metadata_dir}{\code{"data-raw/"}}

\item{save_file}{Defaults to \code{TRUE} and saves raw data conversion to
labelled variables into a .lab file. This can be used as a temporary file to
speed up operations.}

\item{see_log}{\code{TRUE} which will print messages to the screen.}

\item{create_log}{It will create log files in the sr_logs director.}

\item{log_prefix}{Defaults to \code{NA}, in which case a
new one will be assigned to the logs (if they are requested.) The
log_prefix can be directly assigned.}

\item{log_id}{Defaults to \code{NA}, in which case a
new one will be assigned to the logs (if they are requested.) The
log_prefix can be directly assigned.}

\item{my_treshold}{Can be \code{futile.logger::WARN},
\code{futile.logger::INFO}, \code{futile.logger::ERROR}.Defaults to
\code{futile.logger::INFO}.}
}
\description{
Tries to find a data file based on ZACAT ID  on a data directory path,
or, if omitted, on the current \code{tempdir()}. Then it tries to read in
the file and return it.
}
\examples{
\dontrun{
##use your own file path:
zacat_id_data ( zacat_id = "ZA4744",
                metadata_dir = data_dir,
                see_log = see_log,
                create_log = creat_log,
                my_treshold = my_treshold )
}
}
