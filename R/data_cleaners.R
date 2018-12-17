#' Ensure that a vector has only a single value throughout.
#'
#' Missing values are replaced with the single value, and if all values are
#' missing, the first value in \code{missing} is used throughout.
#'
#' @param x The vector which should have a single value
#' @param missing The vector of values to consider missing in \code{x}
#' @param warn_if_all_missing Generate a warning if all values are missing?
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

#' Extract numeric, below/above the lower/upper limits of quantification (LLQ
#' and ULQ), and text data from a vector.
#'
#' @details If \code{llq_pattern} or \code{ulq_pattern} are regular expressions
#'   that extract a value, then the extracted value will be used in the
#'   "llq"/"ulq" column.  If not, then the value \code{-1} will be inserted.  If
#'   all "llq"/"ulq" values match the \code{number_pattern}, then the
#'   "llq"/"ulq" columns will be converted to numeric (if not, a warning will be
#'   given).
#'
#' @param x The character vector to extract data from.
#' @param llq_pattern,ulq_pattern A regex or vector of regex values for
#'   extracting llq and ULQ.  If a vector, all regexes will be tested.
#' @param number_pattern A regex or vector of regex values for finding numbers.
#' @param replace_llq,replace_ulq The value to place in the "number" column when
#'   a llq/ulq is matched (typically \code{0} and \code{Inf}, respecitvely or
#'   \code{NA_real_} for both).
#' @param ... Parameters passed to \code{grep} and \code{grepl} for
#'   \code{llq_pattern} searching.
#' @return a data.frame with columns named "text", "number", "llq", and "ulq".
#' @examples
#' make_loq(c("1", "A", "<1", ">60"))
#' make_loq(c("1", "A", "<1", ">60"), replace_llq=NA_real_)
#' make_loq(c("1", "A", "<1", ">60"), replace_llq=NA_real_, replace_ulq=NA_real_)
#' make_loq(c("1", "A", "<1", ">ULQ"), replace_llq=NA_real_, replace_ulq=NA_real_)
#' make_loq(c("1", "A", "<1", ">ULQ"), replace_llq=NA_real_, replace_ulq=NA_real_, ulq_pattern=">ULQ")
#' @export
make_loq <- function(x,
                     llq_pattern="^< *\\(? *([0-9]*\\.?[0-9]+) *\\)?$",
                     ulq_pattern="^> *\\(? *([0-9]*\\.?[0-9]+) *\\)?$",
                     number_pattern="^(+-)?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?$",
                     replace_llq=0,
                     replace_ulq=Inf,
                     ...) {
  if (!is.character(x)) {
    stop("x must be a character vector.")
  }
  ret <- data.frame(text=x,
                    number=NA_real_,
                    llq=NA_character_,
                    ulq=NA_character_,
                    stringsAsFactors=FALSE)
  loq_patterns <- list(llq=llq_pattern, ulq=ulq_pattern)
  loq_replacement <- list(llq=replace_llq, ulq=replace_ulq)
  # Extract the llq/ulq data
  for (current_direction in names(loq_patterns)) {
    for (current_pattern in loq_patterns[[current_direction]]) {
      mask_loq_current <- grepl(current_pattern, ret$text, ...)
      if (any(mask_loq_current)) {
        # Update the number column with the appropriate replacement value
        ret$number[mask_loq_current] <-
          loq_replacement[[current_direction]]
        # Update the current LOQ column with the appropriate replacement value
        ret[[current_direction]][mask_loq_current] <-
          gsub(
            pattern=current_pattern,
            replacement="\\1",
            x=ret$text[mask_loq_current],
            ...
          )
        # Set the original value to NA and set the numeric value to 0
        ret$text[mask_loq_current] <- NA_character_
      }
    }
    if (all(grepl_multi_pattern(pattern=number_pattern, x=ret[[current_direction]], ...) |
            is.na(ret[[current_direction]]))) {
      ret[[current_direction]] <- as.numeric(ret[[current_direction]])
    } else {
      warning("Not all ", current_direction, " values are numeric, not converting.")
    }
  }
  # Extract the numeric data
  number_mask <- grepl_multi_pattern(pattern=number_pattern, x=ret$text, ...)
  if (any(number_mask)) {
    ret$number[number_mask] <- as.numeric(x[number_mask])
    ret$text[number_mask] <- NA_character_
  }
  ret
}

#' Determine duplicate elements (including the first duplicate)
#'
#' @param x A vector, data.frame, an array, or NULL.
#' @param ... Passed to \code{duplicated}
#' @param fromLast Ignored (included as an argument to prevent passing to
#'   \code{duplicated})
#' @return A vector the same length as the return from \code{duplicated} with
#'   the first duplicated value also flagged rather than just values from the
#'   second to the last (as is returned from \code{duplicated}).
#' @export
duplicated_including_first <- function(x, ..., fromLast=NULL) {
  duplicated(x, ..., fromLast=FALSE) | duplicated(x, ..., fromLast=TRUE)
}
