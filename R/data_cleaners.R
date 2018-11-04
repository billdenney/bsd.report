#' Ensure that a vector has only a single value throughout.
#'
#' Missing values are replaced with the single value, and if all values are
#' missing, the first value in \code{missing} is used throughout.
#'
#' @param x The vector which should have a single value
#' @param missing The vector of values to consider missing in \code{x}
#' @param info If more than one value is found, append this to the warning or
#'   error to assist with determining the location of the issue.
#' @return \code{x} as the scalar single value found throughout (or an error if
#'   more than one value is found).
#' @examples
#' single_value(c(NA, 1))
#' @export
single_value <- function(x, missing=NA, warn_if_all_missing=FALSE, info=NULL) {
  mask_found <- !(x %in% missing)
  found_values <- unique(x[mask_found])
  if (length(found_values) == 0) {
    missing[1]
  } else if (length(found_values) == 1) {
    found_values
  } else {
    if (!is.null(info)) {
      info <- paste(":", info)
    }
    stop("More than one (", length(found_values), ") value found (", paste(found_values, collapse=", "), ")", info)
  }
}

#' Round a value to a fraction, optionally then rounding to a number of digits.
#'
#' @param x The number to round
#' @param denominator The denominator of the fraction for rounding
#' @param digits The number of digits for rounding after rounding to the
#'   fraction (\code{Inf} indicates no subsequent rounding)
#' @return x rounded to a decimal value that has an integer numerator relative
#'   to \code{denominator} (possibly subsequently rounded to a number of decimal
#'   digits).
#' @examples
#' round_to_fraction(0.7, 3)
#' @export
round_to_fraction <- function(x, denominator, digits = Inf) {
  ret <- round(x * denominator, digits=0) / denominator
  if (!is.infinite(digits)) {
    ret <- round(ret, digits = digits)
  }
  ret
}

#' Find the columns that have more than one value
#' 
#' @param x The object (typically a data.frame) with columns to check.
#' @param keep_anyway A character vector of columns to keep even if they
#'   only have a single value.
#' @param ... ignored (for now)
#' @return An object of the same class as \code{x} with boring columns
#'   taken out and the duplicated values placed in an attribute called
#'   \code{"boring"}.
#' @examples
#' interesting_cols(data.frame(A=1:2, B=2))
#' attr(interesting_cols(data.frame(A=1:2, B=2)), "boring")
#' @export
interesting_cols <- function(x, keep_anyway=character(0), ...) {
  if (nrow(x) > 0) {
    # drop the boring columns
    mask_boring <-
      sapply(x,
             FUN=function(y) all(y %in% y[[1]]))
    mask_boring <-
      mask_boring &
      !(names(x) %in% keep_anyway)
    boring <- x[1, mask_boring, drop=FALSE]
    ret <- x[, !mask_boring, drop=FALSE]
    
    # drop the repeated columns
    repeats <- list()
    current_col <- 1
    while (current_col < ncol(ret)) {
      current_repeats <- character(0)
      for (compare_col in ncol(ret):(current_col+1)) {
        if (names(ret)[compare_col] %in% keep_anyway) {
          # do nothing
        } else if (identical(ret[[current_col]],
                             ret[[compare_col]])) {
          current_repeats <- c(current_repeats, names(ret)[compare_col])
          ret <- ret[,-compare_col,drop=FALSE]
        }
      }
      if (length(current_repeats) > 0) {
        repeats[[names(ret)[current_col]]] <- current_repeats
      }
      current_col <- current_col + 1
    }
  } else {
    ret <- x
    boring <- data.frame()
    repeats <- list()
  }
  attr(ret, "boring") <- boring
  attr(ret, "repeats") <- repeats
  ret
}

#' Bidirectional setdiff rather than the default unidirectional version
#' 
#' @param x,y As in \code{setdiff}
#' @return A vector of the two items with setdiff run in both 
#'   directions.  Items are identified from the original set by names
#'   starting with "x" and "y".
#' @examples
#' setdiff_bidir(1:5, 3:8)
#' @export
setdiff_bidir <- function(x, y) {
  c(x=setdiff(x, y), y=setdiff(y, x))
}

#' Extract numeric, BLQ, and text data from a vector.
#' 
#' @param x The character vector to extract data from.
#' @param blq_pattern A regex or vector of regex values for extracting 
#'   BLQ.  If a vector, all regexes will be tested.
#' @param ... Parameters passed to \code{grep} and \code{grepl} for
#'   \code{blq_pattern} searching.
#' @return a data.frame with columns named "text", "number", and "blq".
#' @examples
#' make_blq(c("1", "A", "<1"))
#' @export
make_blq <- function(x,
                     blq_pattern="^< *\\(? *([0-9]*\\.?[0-9]+) *\\)?$",
                     number_pattern="^(+-)?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?$",
                     ...) {
  if (!is.character(x)) {
    stop("x must be a character vector.")
  }
  ret <- data.frame(text=x,
                    number=NA_real_,
                    blq=NA_real_,
                    stringsAsFactors=FALSE)
  # Extract the BLQ data
  for (i in seq_along(blq_pattern)) {
    mask_blq_current <- grepl(blq_pattern[[i]], ret$text, ...)
    if (any(mask_blq_current)) {
      ret$blq[mask_blq_current] <- 0
      blq_value_current <-
        as.numeric(
          gsub(blq_pattern[[i]],
               "\\1",
               ret$text[mask_blq_current],
               ...))
      mask_blq_value <- !is.na(blq_value_current)
      if (any(mask_blq_value)) {
        ret$blq[mask_blq_current][mask_blq_value] <-
          blq_value_current[mask_blq_value]
      }
      # Set the original value to NA and set the numeric value to 0
      ret$text[mask_blq_current] <- NA_character_
      ret$number[mask_blq_current] <- 0
    }
  }
  # Extract the numeric data
  for (i in seq_along(number_pattern)) {
    number_mask_current <- grepl(number_pattern[[i]], ret$text, ...)
    if (any(number_mask_current)) {
      ret$number[number_mask_current] <-
        as.numeric(x[number_mask_current])
      ret$text[number_mask_current] <- NA_character_
    }
  }
  ret
}
