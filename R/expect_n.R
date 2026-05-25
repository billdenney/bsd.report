#' Expect a certain number of values in a logical vector
#'
#' @param x The logical vector
#' @param n The number of expected values
#' @param msg An optional message to display in case of an error
#' @returns `x` if `sum(x) == n` and raises an informative error, otherwise
#' @export
expect_n <- function(x, n = 1, msg = NULL) {
  orig_argument <- deparse1(substitute(x))
  if (is.null(msg)) {
    msg <- orig_argument
  } else {
    msg <- paste(msg, orig_argument, sep = "; ")
  }
  if (any(is.na(x))) {
    stop("`x` may not be NA: ", msg)
  } else if (!is.logical(x)) {
    stop("`x` must be a logical vector: ", msg)
  } else if (sum(x) != n) {
    stop("Expected ", n, " TRUE values but got ", sum(x), " TRUE values: ", msg)
  }
  x
}
