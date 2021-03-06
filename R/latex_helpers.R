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
