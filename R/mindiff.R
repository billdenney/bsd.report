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
#' @importFrom tidyr fill
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
  df <- data.frame(
    value      = c(choices, x),
    choice_val = c(choices, rep(NA_real_, length(x))),
    is_x       = c(rep(FALSE, length(choices)), rep(TRUE, length(x))),
    orig_idx   = c(rep(NA_integer_, length(choices)), seq_along(x))
  )
  # Sort by value.  At ties, choices sort before x when include_zero=TRUE so
  # a measurement exactly at a dose time gives TAD=0; x sorts before choices
  # when include_zero=FALSE to exclude the coincident dose.
  if (include_zero) {
    df <- df[order(df$value, df$is_x), ]
  } else {
    df <- df[order(df$value, !df$is_x), ]
  }
  # Forward-fill the most recent choice value to every subsequent row.
  df <- tidyr::fill(df, "choice_val", .direction="down")
  # Compute TAD for x rows and restore original order.
  x_rows <- df[df$is_x, ]
  result  <- x_rows$value - x_rows$choice_val
  if (none == "negative") {
    result[is.na(result)] <- x_rows$value[is.na(result)] - min(choices)
  }
  result[order(x_rows$orig_idx)]
}
