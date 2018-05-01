#' \code{eurobarometer} package
#'
#' Eurobarometer survey harmonization package
#'
#' See the README on
#' \href{https://github.com/antaldaniel/eurobarometer#readme}{GitHub}
#'
#' @docType package
#' @name eurobarometer
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## vocabulary is a data object that is essential to the working of the package
## and should not be changed (any suggestion to be reported on GitHub)

if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "vocabulary"),
  package="eurobarometer", add=FALSE)
