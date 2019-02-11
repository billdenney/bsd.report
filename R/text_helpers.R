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

#' Make a confidence interval from a point estimate and standard error.
#'
#' @param point The point estimate (numeric vector)
#' @param se The standard error of the estimate (numeric vector)
#' @param level The confidence level
#' @param transform A function that takes a matrix input and returns a matrix
#'   output of the confidence interval values transformed.
#' @param numeric Should the transformed point estimate be returned instead of
#'   the character representation?
#' @return If \code{numeric=FALSE} a character vector of the confidence
#'   intervals represented as "X [X, X]" where X are numbers with three
#'   significant figures (or if the transform returns a character matrix, those
#'   text).  If \code{numeric=TRUE}, a numeric vector of the point estimates.
#' @export
make_ci <- function(point, se, level=0.95, transform=NULL, numeric=FALSE) {
  values <- cbind(point, point + outer(qnorm(p=0.5+level/2)*se, c(-1, 1), FUN=`*`))
  if (!is.null(transform)) {
    values <- transform(values)
  }
  if (numeric) {
    ret <- values[,1]
  } else if (is.numeric(values)) {
    ret <-
      ifelse(
        is.na(values[,2]),
        sprintf("%0.3g [%0.3g]", values[,1], NA),
        sprintf("%0.3g [%0.3g, %0.3g]", values[,1], values[,2], values[,3])
      )
    ret[is.na(point)] <- NA_character_
  } else if (is.character(values)) {
    ret <-
      ifelse(
        is.na(values[,2]),
        sprintf("%s [%s]", values[,1], NA),
        sprintf("%s [%s, %s]", values[,1], values[,2], values[,3])
      )
    
    ret[is.na(point)] <- NA_character_
  }
  ret
}

