#' Generate a latex label or reference to the label
#' @param x The label (a character vector, usually starting with "fig:" for figures and "tab:" for tables)
#' @return A knitr asis_output label or reference
#' @export
latex_reference <- function(x) {
  knitr::asis_output(sprintf("\\ref{%s}", Hmisc::latexTranslate(x)))
}
#' @describeIn latex_reference Generate a latex label (location to be found by a reference).
#' @export
latex_label <- function(x) {
  knitr::asis_output(sprintf("\\label{%s}", Hmisc::latexTranslate(x)))
}
