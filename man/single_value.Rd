% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_cleaners.R
\name{single_value}
\alias{single_value}
\title{Ensure that a vector has only a single value throughout.}
\usage{
single_value(x, missing = NA, warn_if_all_missing = FALSE, info = NULL)
}
\arguments{
\item{x}{The vector which should have a single value}

\item{missing}{The vector of values to consider missing in \code{x}}

\item{warn_if_all_missing}{Generate a warning if all values are missing?}

\item{info}{If more than one value is found, append this to the warning or
error to assist with determining the location of the issue.}
}
\value{
\code{x} as the scalar single value found throughout (or an error if
  more than one value is found).
}
\description{
Missing values are replaced with the single value, and if all values are
missing, the first value in \code{missing} is used throughout.
}
\examples{
single_value(c(NA, 1))
}
