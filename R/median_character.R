#' Return the median of a character vector
#' 
#' @details As half-way between two values in  a character vector is not
#'   defined, in the case of a median that would be between two values, either
#'   the lower or upper value is selected.
#' @inheritParams stats::median
#' @param tie Are ties broken using the upper or lower value?
#' @param ... Passed to `sort()`
#' @return The median value of the vector
#' @export
median_character <- function(x, na.rm=TRUE, tie=c("lower", "upper"), ...) {
  tie <- match.arg(tie)
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    # Match behavior from stats::median() for NA
    return(x[FALSE][NA])
  }
  n <- length(x)
  if (n == 0) {
    x[FALSE][NA]
  } else {
    half <- (n + 1L) %/% 2L
    if (tie == "upper" & (n %% 2 == 0)) {
      half <- half + 1
    }
    sort(x, partial=half, ...)[half]
  }
}
