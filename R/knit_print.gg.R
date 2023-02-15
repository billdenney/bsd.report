#' Print a ggplot object with space around it
#'
#' @param x The plot object
#' @param ... Passed to \code{print}.
#' @param fig_prefix See \code{fig_suffix}
#' @param fig_suffix Character strings passed to \code{cat} before
#'   and after printing \code{x} (if not missing).
#' @param filename Save the figure to the filename, if provided
#' @param width,height,units passed to \code{ggplot2::ggsave()}
#' @return \code{x} invisibly
#' @seealso \code{\link{knit_print.gg_list}}
#' @export
knit_print.gg <- function(x, ..., fig_prefix, fig_suffix, filename=NULL, width=6, height=4, units="in") {
  cat("\n\n")
  if (!missing(fig_prefix)) {
    cat(fig_prefix)
  }
  print(x, ...)
  if (!is.null(filename)) {
    ggplot2::ggsave(
      filename=filename,
      plot=x,
      width=width, height=height, units=units
    )
  }
  if (!missing(fig_suffix)) {
    cat(fig_suffix)
  }
  cat("\n\n")
  invisible(x)
}

#' Print a list of ggplot objects with space around each
#' @param x A list of plot objects
#' @param ... Passed to \code{knit_print}.
#' @param filename Save the figure to the filename, if provided.  If the
#'   filename contains "%d" (optionally with sprintf-formatting instructions
#'   such as "%03d"), then the filename will be run through
#'   \code{sprintf(filename, seq_along(x))} to generate the filename.
#' @inheritParams knit_print.gg
#' @return \code{x} invisibly
#' @seealso \code{\link{knit_print.gg}}
#' @export
knit_print.gg_list <- function(x, ..., filename=NULL, fig_suffix="\n\n") {
  if (!is.null(filename)) {
    if (grepl(x=filename, pattern="%[0-9]*d")) {
      filename <- sprintf(filename, seq_along(x))
    }
  }
  stopifnot("`filename` must be either NULL or the same length as `x`"=is.null(filename) | length(filename) == length(x))
  lapply(
    X=seq_along(x),
    FUN=function(idx) {
      knit_print(
        x=x[[idx]],
        ...,
        filename=filename[[idx]],
        fig_suffix=fig_suffix
      )
    }
  )
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
  x
}
