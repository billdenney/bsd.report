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
  Hmisc::latexTranslate(gsub(pattern=" ", replacement="-", x=x))
}

#' Append a caption to a string vector of captions
#'
#' @param x The list of captions to add to.
#' @param value a vector with length 1 or 2.  If \code{length(value) == 1}, it
#'   is the caption text.  If \code{length(value) == 2}, the first element is
#'   the caption text and the second is the label for the figure (to be passed
#'   into \code{latex_label}).
#' @return \code{c(x, paste0(latex_label(value[2]), value[1]))} or \code{c(x, value)}
#' @export
`add_caption<-` <- function(x, value) {
  c(
    x,
    if (length(value) == 2) {
      paste0(latex_label(value[2]), value[1])
    } else if (length(value) == 1) {
      value
    } else {
      stop("value must have length 1 or 2.")
    }
  )
}
