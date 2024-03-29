% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gesis_shortname_harmonization.R
\name{gesis_shortname_harmonization}
\alias{gesis_shortname_harmonization}
\title{GESIS shortname harmonization}
\usage{
gesis_shortname_harmonization(short_name, long_name)
}
\arguments{
\item{short_name}{The metadata$spss_name}

\item{long_name}{The updated metadata$suggested_name}
}
\description{
Using the data in the short variable names (codes) to correctly assign a
long variable name
}
\examples{
\dontrun{
 gesis_shortname_harmonization (short_name = "wextra",
                                long_name  = "Weight extrapolated 15+")
}
}
