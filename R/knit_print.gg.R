#' Print a ggplot object with space around it
#' 
#' @param x The plot object
#' @return \code{x} invisibly
#' @seealso \code{\link{knit_print.gg_list}}
#' @export
knit_print.gg <- function(x, ..., fig_prefix, fig_suffix) {
  cat("\n\n")
  if (!missing(fig_prefix)) {
    cat(fig_prefix)
  }
  print(x, ...)
  if (!missing(fig_suffix)) {
    cat(fig_suffix)
  }
  cat("\n\n")
  invisible(x)
}

#' Print a list of ggplot objects with space around each
#' @param x A list of plot objects
#' @return \code{x} invisibly
#' @seealso \code{\link{knit_print.gg}}
#' @export
knit_print.gg_list <- function(x, ..., fig_suffix="\\clearpage\n") {
  lapply(X=x, FUN=knit_print, ..., fig_suffix=fig_suffix)
  invisible(x)
}

#' Make a gg_list object (a list of ggplots) for knit_printing
#' 
#' @param x The list
#' @return A gg_list object (just adding the class to the list)
#' @export
as_gg_list <- function(x) {
  if (is.list(x)) {
    class(x) <- "gg_list"
  } else {
    stop("Not a list")
  }
}