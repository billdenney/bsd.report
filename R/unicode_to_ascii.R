#' Convert from Unicode to ASCII
#' 
#' @param x Object that may contain character strings for conversion.
#' @param verbose Provide messages about number of values changed.
#' @return An object of the same class as \code{x} with Unicode characters
#'   converted to ASCII.
#' @export
unicode_to_ascii <- function(x, ...) {
  UseMethod("unicode_to_ascii")
}

#' @rdname unicode_to_ascii
#' @export
unicode_to_ascii.default <- function(x, verbose=FALSE, ...) {
  if (verbose) {
    message("Returning unchanged: Cannot convert from Unicode to ASCII for class ", class(x)[1])
  }
  x
}

#' @rdname unicode_to_ascii
#' @param pattern,replacement,... Passed to \code{stringi::stri_replace_all_fixed()}
#' @param general_conversion After the initial, specific conversions, run
#'   \code{stringi::stri_trans_general(x,
#'   id="Any-Latin;Greek-Latin;Latin-ASCII")} on the current strings.
#' @export
#' @importFrom stringi stri_replace_all_fixed
unicode_to_ascii.character <- function(x, verbose=FALSE,
                                       pattern=c("\u03bc", "\u00b5"),
                                       replacement=c("u", "u"),
                                       general_conversion=TRUE, ...) {
  ret <-
    stringi::stri_replace_all_fixed(
      str=x, pattern=pattern, replacement=replacement, vectorize_all=FALSE,
      ...
    )
  if (general_conversion) {
    ret <- stringi::stri_trans_general(ret, id="Any-Latin;Greek-Latin;Latin-ASCII")
  }
  if (verbose) {
    changes <-
      sum(is.na(x) != is.na(ret)) +
      sum(x[!is.na(x)] != ret[!is.na(x)])
    if (changes > 0) {
      message(
        changes,
        ngettext(
          changes,
          " change for Unicode to ascii conversion.",
          " changes for Unicode to ascii conversion."
        )
      )
    } else {
      message("No Unicode to ascii changes.")
    }
  }
  ret
}

#' @rdname unicode_to_ascii
#' @export
unicode_to_ascii.factor <- function(x, ...) {
  levels(x) <- unicode_to_ascii(levels(x), ...)
  x
}

#' @rdname unicode_to_ascii
#' @export
unicode_to_ascii.logical <-
  unicode_to_ascii.NULL <-
  unicode_to_ascii.numeric <-
  function(x, verbose=FALSE, ...) {
    if (verbose) message("No Unicode to ascii changes for class: ", class(x)[1])
    x
  }

#' @rdname unicode_to_ascii
#' @export
unicode_to_ascii.data.frame <- function(x, verbose=FALSE, ...) {
  for (idx in seq_along(x)) {
    if (verbose) message("Unicode to ascii current column number and name: ", idx, ", `", names(x)[idx], "`")
    x[[idx]] <- unicode_to_ascii(x[[idx]], verbose=verbose, ...)
  }
  x
}

#' @rdname unicode_to_ascii
#' @export
unicode_to_ascii.list <- function(x, verbose=FALSE, ...) {
  x_names <-
    if (is.null(names(x))) {
      rep("NULL", length(x))
    } else {
      names(x)
    }
  for (idx in seq_along(x)) {
    if (verbose) message("Unicode to ascii current list element number and name: ", idx, ", `", x_names[idx], "`")
    x[[idx]] <- unicode_to_ascii(x[[idx]], verbose=verbose, ...)
  }
  x
}
