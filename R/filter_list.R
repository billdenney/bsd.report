#' Filter if it works, otherwise, do nothing
#'
#' @param .data The data to potentially filter
#' @param ... Arguments (see dplyr::filter)
#' @param .ignore_errors (always true)
#' @return Either NULL if an error or the filtered results.
#' @seealso \code{\link{filter_list}}
#' @importFrom dplyr filter
#' @export
filter_maybe <- function(.data, ..., .ignore_errors = TRUE) {
  tryCatch(
    dplyr::filter(.data, ...),
    error = function(e) NULL
  )
}

#' Filter a list of data.frames
#'
#' @param .data A list of data.frames (or similar)
#' @param ... Arguments to filter_maybe (eventually passed to dplyr::filter)
#' @param .ignore_errors (always true)
#' @return A list of data.frames after filtering-- or NULL
#' @seealso \code{\link{filter_maybe}}
#' @examples
#' filter_list(list(mtcars, iris), cyl == 6)
#' @export
filter_list <- function(.data, ..., .ignore_errors = TRUE) {
  lapply(.data, FUN = filter_maybe, ...)
}
