% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_numeric.factor_5.R
\name{as_numeric.factor_5}
\alias{as_numeric.factor_5}
\title{Convert to numeric from a factor_4 object}
\usage{
\method{as_numeric}{factor_5}(x, type = NULL)
}
\arguments{
\item{x}{An object to be converted to numeric}

\item{type}{Default to \code{NULL}. Not used in this instance
of the method.}
}
\description{
Convert to numeric from a factor_4 object
}
\examples{
my_categories <-  as_factor_5( c("DK", "Much lower",
       "Somewhat lower", "More or less the same",
       "Somewhat higher", "Much higher")
       )

as_numeric (my_categories)

}
