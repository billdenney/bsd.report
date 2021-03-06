% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_manage.R
\name{check_expected_cols}
\alias{check_expected_cols}
\title{Verify that a data.frame has the expected columns present}
\usage{
check_expected_cols(data, cols)
}
\arguments{
\item{data}{A data.frame or similar object}

\item{cols}{A character vector of expected column names}
}
\value{
`data` where the columns are ordered according to the order in
  `cols`.
}
\description{
Verify that a data.frame has the expected columns present
}
\details{
Either more or fewer columns are an error.
}
\seealso{
Other Data Management: 
\code{\link{get_data_manage_standard_cols}()},
\code{\link{nonmem_column_order}()}
}
\concept{Data Management}
