% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_factor.factor_yes_no_4.R
\name{as_factor.factor_yes_no_4}
\alias{as_factor.factor_yes_no_4}
\title{Convert to factor from a factor_yes_no_4 object}
\usage{
\method{as_factor}{factor_yes_no_4}(x, type = NULL)
}
\arguments{
\item{x}{An object to be converted to factor}

\item{type}{Default to \code{NULL}. Not used in this instance
of the method.}
}
\description{
Convert to factor from a factor_yes_no_4 object
}
\examples{
my_categories <- as_factor_yes_no_4( c("Very good", "Rather Good",
                  "Inap.", "Rather Bad", "DK", "Very bad"))

as_factor (my_categories)

}
