#' Concatenate strings dropping missing values
#'
#' @details If all values are missing, the value from the first argument is
#'   preserved.  \code{paste_last} affects the final output; the main difference
#'   is that if \code{FALSE}, \code{NA_character_} values will be preserved, and
#'   if \code{TRUE}, \code{NA_character_} values will be converted to "NA" (as
#'   is the case with \code{paste()}).
#'
#' @inheritParams base::paste
#' @param missing_values Values considered missing to be ignored in pasting.
#' @param paste_last When all \code{...} arguments have been combined and only
#'   one remains, should \code{paste} be called on that last argument?  (Ignored
#'   if \code{collapse} is not \code{NULL}.)
#' @return A character vector of pasted values.
#' @export
paste_missing <- function(..., sep=" ", collapse=NULL, missing_values=NA, paste_last=FALSE) {
  args <- list(...)
  if (length(args) <= 1) {
    if (length(args) == 0) {
      # match the behavior of paste
      paste(sep=sep, collapse=collapse)
    } else if (paste_last | !is.null(collapse)) {
      paste(..., sep=sep, collapse=collapse)
    } else {
      args[[1]]
    }
  } else {
    # There are at least 2 arguments; collapse the first two and recurse
    a1 <- args[[1]]
    a2 <- args[[2]]
    if (length(a1) != length(a2)) {
      if (length(a1) == 1) {
        a1 <- rep(a1, length(a2))
      } else if (length(a2) == 1) {
        a2 <- rep(a2, length(a1))
      } else {
        stop("Arguments must be the same length or one argument must be a scalar.")
      }
    }
    mask1 <- !(args[[1]] %in% missing_values)
    mask2 <- !(args[[2]] %in% missing_values)
    mask_both <- mask1 & mask2
    mask_only2 <- (!mask1) & mask2
    firsttwo <- args[[1]]
    if (any(mask_only2)) {
      firsttwo[mask_only2] <- args[[2]][mask_only2]
    }
    if (any(mask_both)) {
      # Collapse only occurs on the final pasting
      firsttwo[mask_both] <- paste(args[[1]][mask_both], args[[2]][mask_both], sep=sep, collapse=NULL)
    }
    new_args <- append(list(firsttwo), args[-(1:2)])
    new_args$sep <- sep
    new_args$collapse <- collapse
    new_args$missing_values <- missing_values
    new_args$paste_last <- paste_last
    do.call(paste_missing, new_args)
  }
}
