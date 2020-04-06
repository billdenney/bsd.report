#' Convert a power calculation to a table for reporting.
#' 
#' @param x A `power.htest` object to convert to a data.frame.
#' @param ... Ignored
#' @param digits Significant digits to report for numeric values.
#' @method as.data.frame power.htest
#' @export
#' @importFrom dplyr recode
as.data.frame.power.htest <- function(x, ..., digits=3) {
  browser()
  value <-
    sapply(
      X=x,
      FUN=function(y) {
        if (is.numeric(y)) {
          as.character(signif(y, digits=digits))
        } else {
          y
        }
      }
    )
  data.frame(
    Parameter=
      dplyr::recode(
        names(x),
        n="N",
        sd="SD",
        sig.level="Significance Level",
        power="Power",
        alternative="Alternative",
        note="Note",
        method="Method"
      ),
    Value=
      dplyr::recode(
        unname(value),
        one.sided="One-sided",
        two.sided="Two-sided"
      ),
    stringsAsFactors=FALSE
  )
}
