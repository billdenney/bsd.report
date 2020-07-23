#' Perform an operation on a vector that may be empty or all NA with
#' controlled output.
#' 
#' @param x The object to perform the operation on.
#' @param FUN The function to perform if the object is neither zero length or all \code{NA}
#' @param zero_len If \code{x} has length 0, then either return it (if \code{NULL}) or return \code{NA} of the same class as \code{x} (if \code{NA})
#' @return An object of the same class as \code{x}
#' @examples 
#' min_no_na(c(NA, 3))
#' min_no_na(double(), zero_len=NULL)
#' max_no_na(c(NA, 3))
#' @export
fun_no_na <- function(x, FUN, zero_len=NULL) {
  if (!length(x)) {
    if (is.null(zero_len)) {
      x
    } else if (is.na(zero_len)) {
      # return NA in the same type
      x[NA]
    } else {
      stop("`zero_len` must be `NULL` or `NA`.")
    }
  } else if (all(is.na(x))) {
    x[NA]
  } else {
    FUN(x[!is.na(x)], na.rm=TRUE)
  }
}

#' @describeIn fun_no_na Maximum
#' @export
max_no_na <- function(x, zero_len=NULL) {
  fun_no_na(x, max, zero_len=zero_len)
}

#' @describeIn fun_no_na Minimum
#' @export
min_no_na <- function(x, zero_len=NULL) {
  fun_no_na(x, min, zero_len=zero_len)
}

#' @describeIn fun_no_na Median
#' @export
#' @importFrom stats median
median_no_na <- function(x, zero_len=NULL) {
  fun_no_na(x, stats::median, zero_len=zero_len)
}

#' @describeIn fun_no_na Median
#' @export
mean_no_na <- function(x, zero_len=NULL) {
  fun_no_na(x, base::mean, zero_len=zero_len)
}

#' @describeIn fun_no_na Median
#' @export
#' @importFrom stats sd
sd_no_na <- function(x, zero_len=NULL) {
  fun_no_na(x, stats::sd, zero_len=zero_len)
}
