#' Generate a latex label or reference to the label
#' @param x The label (a character vector, usually starting with "fig:" for figures and "tab:" for tables)
#' @return A knitr asis_output label or reference
#' @export
latex_reference <- function(x) {
  knitr::asis_output(
    sprintf(
      "\\ref{%s}",
      latex_label_clean(x)
    )
  )
}
#' @describeIn latex_reference Generate a latex label (location to be found by a reference).
#' @export
latex_label <- function(x) {
  knitr::asis_output(
    sprintf(
      "\\label{%s}",
      latex_label_clean(x)
    )
  )
}
#' @describeIn latex_reference Clean latex labels so that they are usable
#'   (remove spaces and pass through \code{Hmisc::latexTranslate}).
#' @export
#' @importFrom Hmisc latexTranslate
latex_label_clean <- function(x) {
  gsub(x=x, pattern="[^A-Za-z0-9:-]+", replacement="-")
}

#' @describeIn latex_reference Generate a latex label for the first and last of
#'   a list of objects with blanks between
#' @param object The object (with a length > 1)
#' @param text The text to use for the start of the label (usually "fig:xxx" or
#'   "tab:xxx")
#' @param sep The separator to use between the text and the suffix
#' @param suffix_text a 2-long character string with the text to use after the
#'   separator
#' @return A character vector with latex labels for the first and last objects
#'   and blank strings in between.
#' @export
latex_label_first_last <- function(object, text, sep="-", suffix_text=c("first", "last")) {
  stopifnot("object must be >1 long"=length(object) > 1)
  stopifnot("text must be a scalar"=length(text) == 1)
  stopifnot("sep must be a scalar"=length(sep) == 1)
  stopifnot("suffix_text must be 2 long"=length(suffix_text) == 2)
  idx_first <- 1
  idx_last <- length(object)
  ret <- rep("", length(object))
  ret[idx_first] <- bsd.report::latex_label(paste(text, suffix_text[1], sep=sep))
  ret[idx_last] <- bsd.report::latex_label(paste(text, suffix_text[2], sep=sep))
  ret
}
