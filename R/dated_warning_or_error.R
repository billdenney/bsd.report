#' Give a warning or error based on the current date
#'
#' @param date The date cutoff
#' @param ... Passed to \code{stop} if todays date is < \code{date} or
#'   \code{warning} otherwise.
#' @export
dated_warning_or_error <- function(date, ...) {
  if (as.character(Sys.Date()) > as.character(date)) {
    stop(...)
  } else {
    warning(...)
  }
}
