#' Generate a list of text grammatically correctly
#'
#' @param x a vector to be put into a list.
#' @param oxford_comma Should the Oxford comma be used?
#' @param conjunction The conjunction to use (without spaces).
#'   \code{conjunction} will usually be "and" or "or".
#' @return A character string of the vector input separated by commas and the
#'   word "and" as appropriate.
#' @export
comma_and <- function(x, oxford_comma=TRUE, conjunction="and") {
  ret <- as.character(x)
  if (length(ret) == 0) {
    warning("comma.and was given zero-length input")
    ret <- ""
  } else if (length(ret) == 1) {
    ## Do nothing
  } else if (length(ret) == 2) {
    ## Only put an "and" between the inputs
    ret <- paste(ret, collapse=paste("", conjunction, ""))
  } else {
    ## Put commas between most of the entries, put "and" between the last two,
    ## and put an Oxford comma in before "and" if requested.
    thisand <- paste("", conjunction, "")
    if (oxford_comma) {
      thisand <- paste(",", conjunction, "")
    }
    ret <- paste(paste(ret[-length(ret)], collapse=", "),
                 ret[length(ret)], sep=thisand)
  }
  ret
}
