% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_numeric.factor_binary.R
\name{as_numeric.factor_binary}
\alias{as_numeric.factor_binary}
\title{Convert to numeric from a factor_binary object}
\usage{
\method{as_numeric}{factor_binary}(x, type = NULL)
}
\arguments{
\item{x}{An object to be converted}

\item{type}{Default to \code{NULL}. Not used in this instance
of the method.}
}
\description{
Convert to numeric from a factor_binary object
}
\examples{
my_binary <- as_factor_binary(c("mentioned", "mentioned",
 "not mentioned", "dk"))

as_numeric (my_binary)

}
