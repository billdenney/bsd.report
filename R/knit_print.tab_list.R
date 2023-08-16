#' Create a tab_list object that enables simpler printing of lists of tables
#'
#' @param x A data.frame or similar object with at least columns named "table" and "caption"
#' @return `x` with a class of "tab_list" added to its list of classes
#' @export
as_tab_list <- function(x) {
  if (is.data.frame(x) && all(c("table", "caption") %in% names(x))) {
    class(x) <- c("tab_list", class(x))
  } else {
    stop("Not a data.frame with 'table' and 'caption' columns")
  }
  x
}

#' Print a `tab_list` object
#'
#' @inheritParams knit_print.gg_list
#' @param tab_suffix Character strings passed to `cat()` before and after
#'   printing `x` (if not missing).
#' @param ... passed to `tabfun`
#' @param tabfun The function used to generate a table.  Common examples are
#'   `pander::pandoc.table.return` and `TopicLongTable::topic_long_table`
#' @return The character string to output as a `knitr` `asis_output` object
#' @export
knit_print.tab_list <- function(x, ..., filename = NULL, tab_suffix = "\n\n", tabfun = pander::pandoc.table.return) {
  ret <-
    lapply(
      X = seq_len(nrow(x)),
      FUN = \(idx) {
        tabfun(x$table[[idx]], caption = x$caption[idx], ...)
      }
    )
  ret <- do.call(paste, append(ret, list(collapse = tab_suffix, sep = "")))
  knitr::asis_output(ret)
}
