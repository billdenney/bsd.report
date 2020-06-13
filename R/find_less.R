#' Find the choice that is less than a value
#' 
#' @details \code{x} and \code{choices} must be comparable by the \code{<} or
#'   \code{<=} operators.  \code{choices} will be sorted to be in ascending
#'   order; sorting will remove any \code{NA} values from choices.
#' 
#' @param x The vector of values to select choices for.
#' @param choices The choices that are to be selected from.
#' @param include_same Include if the choice is \code{<=} and not strictly
#'   \code{<}.
#' @param none What if the \code{x} value is less than (or less than or equal to
#'   if \code{include_same}) all the choices?  Choose the \code{"first"} choice
#'   or insert an \code{NA} value?
#' @return A vector with values from \code{choices} that are less than (or equal
#'   to) \code{x}
#' @export
find_less <- function(x, choices, include_same=TRUE, none=c("first", "na")) {
  none <- match.arg(none)
  if (length(x) == 0) {
    warning("An empty vector was given for `x`, returning empty.")
    ret <- choices[c()]
  } else if (all(is.na(choices))) {
    warning("`choices` only contains `NA` values or is empty; returning NA.")
    ret <- choices[NA][seq_along(x)]
  } else {
    choices <- sort(choices)
    ret <- choices[NA][seq_along(x)]
    for (current_choice in choices) {
      mask <-
        !is.na(x) &
        (
          if (include_same) {
            current_choice <= x
          } else {
            current_choice < x
          }
        )
      ret[mask] <- current_choice
    }
    if (none %in% "first") {
      ret[!is.na(x) & is.na(ret)] <- choices[1]
    }
  }
  ret
}
