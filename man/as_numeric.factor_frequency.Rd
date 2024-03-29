% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_numeric.factor_frequency.R
\name{as_numeric.factor_frequency}
\alias{as_numeric.factor_frequency}
\title{Convert to numeric from a factor_frequency object}
\usage{
\method{as_numeric}{factor_frequency}(x, type = NULL)
}
\arguments{
\item{x}{An object to be converted to numeric}

\item{type}{Default to \code{NULL}. Not used in this instance
of the method.}
}
\description{
Convert to numeric from a factor_frequency object
}
\examples{
my_categories <- as_factor_frequency(
 x  = c("Less often", "Several times a month",
 "Every 2-3 months", "Once a month", "DK"))

as_numeric (my_categories)

}
