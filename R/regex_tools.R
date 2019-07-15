#' Substitute from a vector of patterns with a single replacement
#'
#' @param x,replacement See \code{gsub}
#' @param patterns A vector of patterns as used individually in \code{gsub} and
#'   \code{grepl}
#' @param ... Passed to \code{gsub} and \code{grepl}
#' @param verbose Signal messages with the count of values that matched each
#'   pattern or no pattern.
#' @return A vector of \code{NA_character_} when no match occurs and the
#'   replaced value when a match occurs.
#' @seealso \code{\link{grepl_multi_pattern}}
#' @export
gsub_multi_pattern <- function(x, patterns, replacement, ..., verbose=FALSE) {
  ret <- rep(NA_character_, length(x))
  matched <- rep(FALSE, length(x))
  while (length(patterns) & any(!matched)) {
    current_pattern <- patterns[1]
    patterns <- patterns[-1]
    current_match <-
      !matched &
      grepl(pattern=current_pattern, x=x, ...)
    matched <- matched | current_match
    ret[current_match] <- gsub(
      pattern=current_pattern,
      replacement=replacement,
      x=x[current_match],
      ...
    )
    if (verbose) {
      message(
        sum(current_match),
        " values matched the following pattern: ",
        current_pattern
      )
    }
  }
  if (verbose) {
    message(sum(!matched), " values matched no pattern.")
  }
  ret
}

#' Run grepl on a vector of patterns.
#' 
#' @param pattern One or more patterns (see grepl)
#' @param x A string vector
#' @param ... Passed to grepl
#' @return A boolean vector if any pattern is matched
#' @examples
#' grepl_multi_pattern(pattern=c("A", "B", "C"), LETTERS)
#' @seealso \code{\link{gsub_multi_pattern}}
#' @export
grepl_multi_pattern <- function(pattern, x, ...) {
  ret <- rep(FALSE, length(x))
  for (current_pattern in pattern) {
    ret[grepl(pattern=current_pattern, x=x, ...)] <- TRUE
  }
  ret
}

#' Regular expression patterns for numbers
#' 
#' @details
#' \itemize{
#'   \item{natural}{positive integers without a sign}
#'   \item{nonnegative_integer_no_sign}{only digits}
#'   \item{integer}{an optional +- at the beginning followed by only digits
#'     (note that "-0" is valid)}
#'   \item{float_no_sign}{digits, a decimal point, and more digits}
#'   \item{float_or_integer_relaxed}{an optional +- followed by one of the
#'     following:}
#'   \itemize{
#'     \item{digits}
#'     \item{digits then a decimal point}
#'     \item{a decimal point then digits}
#'     \item{digits then a decimal point then digits}
#'   }
#'   \item{float_or_integer_strict}{an optional +- followed by digits optionally
#'     followed by a decimal point and digits (starting or ending with a decimal
#'     point is not allowed).}
#'   \item{scientific_notation}{Strict scientific notation made of the
#'     following, in order:}
#'   \itemize{
#'     \item{an optional +-}
#'     \item{a coefficient that is zero (with an optional decimal point and
#'       optional additinoal zeros), a natural number, or a float that does not
#'       start with zero}
#'     \item{one of "e", "E", "d", or "D"}
#'     \item{a optional +-}
#'     \item{integer}
#'   }
#'   \item{scientific_notation_relaxed}{Relaxed scientific notation (relaxing on
#'     the coefficient rules) made of the following, in order:}
#'   \itemize{
#'     \item{float_or_integer_relaxed}
#'     \item{one of "e", "E", "d", or "D"}
#'     \item{a optional +-}
#'     \item{integer}
#'   }
#'   \item{number_relaxed}{Any number format above (based on
#'     scientific_notation_relaxed with the exponent as an optional component}
#' }
#' @examples
#' grepl(pattern=number_patterns$scientific_notation_relaxed, "1e5")
#' grepl(pattern=number_patterns$scientific_notation_relaxed, "1e-5")
#' grepl(pattern=number_patterns$scientific_notation_relaxed, "1e.5")
#' @export
number_patterns <- list()
number_patterns$natural <- "[1-9][0-9]*"
number_patterns$nonnegative_integer_no_sign <- "[0-9]+"
number_patterns$integer <-
  paste0("[+-]?", number_patterns$nonnegative_integer_no_sign)
number_patterns$float_no_sign <- "[0-9]+\\.[0-9]+"
number_patterns$float_or_integer_relaxed <-
  paste0(
    "[+-]?",
    "(?:",
    number_patterns$nonnegative_integer_no_sign, "|",
    number_patterns$nonnegative_integer_no_sign, "\\.", "|",
    "\\.", number_patterns$nonnegative_integer_no_sign, "|",
    number_patterns$float_no_sign,
    ")"
  )
number_patterns$float_or_integer_strict <-
  paste0("[+-]?",
         number_patterns$nonnegative_integer_no_sign,
         "(?:\\.", number_patterns$nonnegative_integer_no_sign, ")")
number_patterns$scientific_notation <-
  paste0(
    "[+-]?",
    # Coefficient
    "(?:",
      "0(?:\\.0*)?|",
      "[1-9](?:\\.[0-9]*)?",
    ")",
    "[eEdD]",
    number_patterns$integer
  )
number_patterns$scientific_notation_relaxed <-
  paste0(
    number_patterns$float_or_integer_relaxed,
    "[eEdD]",
    number_patterns$integer
  )
number_patterns$number_relaxed <-
  paste0(
    number_patterns$float_or_integer_relaxed,
    "(?:[eEdD]",
    number_patterns$integer,
    ")?"
  )
