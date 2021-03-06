% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_cleaners.R
\name{duplicated_including_first}
\alias{duplicated_including_first}
\title{Determine duplicate elements (including the first duplicate)}
\usage{
duplicated_including_first(x, ..., fromLast = NULL)
}
\arguments{
\item{x}{A vector, data.frame, an array, or NULL.}

\item{...}{Passed to \code{duplicated}}

\item{fromLast}{Ignored (included as an argument to prevent passing to
\code{duplicated})}
}
\value{
A vector the same length as the return from \code{duplicated} with
  the first duplicated value also flagged rather than just values from the
  second to the last (as is returned from \code{duplicated}).
}
\description{
Determine duplicate elements (including the first duplicate)
}
