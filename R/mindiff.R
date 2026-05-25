#' Find the minimum difference between a vector and a set of choices.
#'
#' @param x The vector of values to adjust
#' @param choices The vector of values which should be matched
#' @param tie How should a tie (two choices are equidistant) be managed? (See
#'   details.)
#' @return The vector of \code{x} replaced by the closest values from the
#'   \code{choices} vector.
#' @seealso \code{\link{mindiff_after}}
#' @export
mindiff <- function(x, choices, tie=c("first", "last", "median-first", "median-last")) {
  tie <- match.arg(tie)
  if (length(x) == 0) {
    warning("`x` is zero-length, cannot match to any `choices`.")
    x
  } else if (length(choices) > 0) {
    choices <- sort(choices)
    distances <- abs(sapply(x, FUN="-", choices))
    apply(X=distances,
          MARGIN=2,
          FUN=function(x, tie) {
            ret <- which(x == min(x))
            if (length(ret) > 1) {
              if (tie == "first") {
                ret <- ret[1]
              } else if (tie == "last") {
                ret <- ret[length(ret)]
              } else if (tie == "median-first") {
                ret <- ret[floor(length(ret)/2)]
              } else if (tie == "median-last") {
                ret <- ret[ceiling(length(ret)/2)]
              } else {
                # This should never happen as it should already be caught by
                # match.arg above.
                stop("Invalid value for `tie` argument, please report this as a bug.") # nocov
              }
            }
            choices[ret]
          },
          tie=tie)
  } else {
    stop("No `choices` given.")
  }
}

#' Return the minimum of the differences of x for the first value after the
#' choices.
#'
#' @param x The values to select choices for.
#' @param choices The choices from which to select the minimum value.
#' @param include_zero Should choices only be selected if they are strictly
#'   after or should zero be allowed?
#' @param none What do you do if \code{x < min(choices)}?  Return a
#'   \code{"negative"} value or return \code{NA}?
#' @return A vector the same length as \code{x} with the minimum difference
#'   between \code{x} and any value of \code{choices}.  If \code{choices} has
#'   length 0, then return a vector of \code{NA} the same length as \code{x}
#'   (with a warning).
#' @seealso \code{\link{mindiff}}
#' @export
mindiff_after <- function(x, choices, include_zero=TRUE, none=c("negative", "na")) {
  none <- match.arg(none)
  if (length(x) == 0) {
    warning("`x` is zero-length, cannot match to any `choices`.")
    return(x)
  }
  if (length(choices) == 0) {
    warning("No `choices` given, returning NA")
    return(x[NA])
  }
  choices <- sort(choices)
  # findInterval() is a C-level binary search: for each x[i] it returns the
  # index of the largest choices[j] satisfying the boundary condition.
  # include_zero=TRUE  → largest j where choices[j] <= x[i]  (left.open=FALSE)
  # include_zero=FALSE → largest j where choices[j] <  x[i]  (left.open=TRUE)
  idx <- findInterval(x, choices, left.open=!include_zero)
  # Use pmax to avoid zero-index subscripting (R drops 0-index silently).
  # The result for idx==0 rows is overwritten below.
  result <- x - choices[pmax(idx, 1L)]
  no_prior <- !is.na(idx) & idx == 0L
  if (none == "negative") {
    result[no_prior] <- x[no_prior] - choices[1L]
  } else {
    result[no_prior] <- NA_real_
  }
  result
}
