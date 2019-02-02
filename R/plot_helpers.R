# Technique from http://htmlpreview.github.io/?https://github.com/wilkelab/cowplot/blob/master/inst/doc/shared_legends.html

#' Extract the legend from the first figure, remove the legends from all plots,
#' and put the legend at the end.
#'
#' @param ... One or more ggplot2 objects
#' @return A list of the ggplot2 objects (suitable for the \code{plotlist}
#'   argument of \code{cowplot::plot_grid})
#' @family plot legend helpers
#' @export
plot_grid_one_legend <- function(...) {
  args <- list(...)
  ## Extract the legend from the first plot.
  legend <- extract_ggplot_legend(args[[1]])
  ## Concatenate the plots together
  ret <- lapply(X=args, FUN=remove_ggplot_legend)
  ret[[length(ret) + 1]] <- legend
  ret
}

#' Remove the legend(s) from a ggplot
#' 
#' @param A ggplot2 object
#' @return The object with legends hidden.
#' @family plot legend helpers
#' @export
#' @importFrom ggplot2 theme
remove_ggplot_legend <- function(x) {
  x + ggplot2::theme(legend.position="none")
}

#' Extract the legend from a ggplot2 object
#' 
#' @param x A ggplot2 object
#' @return The legend grob from that object
#' @family plot legend helpers
#' @export
#' @importFrom ggplot2 ggplotGrob
extract_ggplot_legend <- function(x) {
  grobs <- ggplot2::ggplotGrob(x)$grobs
  grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
}
