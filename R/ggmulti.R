#' Make a list of ggplot2 objects that can be added to and have associated captions
#'
#' @param figure The ggplot2 object
#' @param caption The required caption for the figure
#' @export
#' @examples
#' p1 <-
#'   ggplot2::ggplot(data = data.frame(x = 1:2, y = 2:3), ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point()
#' p2 <-
#'   ggplot2::ggplot(data = data.frame(x = 2:3, y = 4:5), ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point()
#' c(
#'   ggmulti(
#'     figure = p1,
#'     caption = "my caption"
#'   ),
#'   ggmulti(
#'     figure = p2,
#'     caption = "my caption 2"
#'   )
#' )
ggmulti <- function(figure, caption) {
  new_ggmulti(figure = figure, caption = caption)
}

#' @export
new_ggmulti <- function(figure = NULL, caption = character()) {
  if (is.null(figure)) {
    figure <- list()
  } else if (inherits(figure, "gg")) {
    figure <- list(figure)
  } else if (length(figure) == 1 && is.na(figure)) {
    # ensure that it's a logical NA
    figure <- NA
  } else {
    stop("'figure' must either be a ggplot2 object or NA")
  }
  if (length(caption) == 0) {
    caption <- character()
  } else {
    vctrs::vec_assert(caption, ptype = character(), size = 1)
    stopifnot(!is.na(caption))
  }
  vctrs::new_rcrd(list(figure = figure, caption = caption), class = "ggmulti")
}

#' @export
is_ggmulti <- function(x) {
  inherits(x, "ggmulti")
}

is.na.ggmulti <- function(x) {
  is.na(vctrs::field(x, "caption"))
}

#' @export
format.ggmulti <- function(x, ...) {
  vctrs::field(x, "caption")
}

#' @export
vec_ptype_abbr.ggmulti <- function(x, ...) {
  "ggmlt"
}

#' @export
vec_ptype2.ggmulti.ggmulti <- function(x, y, ...) {
  new_ggmulti()
}

#' @export
vec_cast.ggmulti.character  <- function(x, to, ...) character()
#' @export
vec_cast.character.ggmulti  <- function(x, to, ...) character()
#' @export
as.character.ggmulti <- function(x, ...) {
  vapply(X = x, FUN = vctrs::field, FUN.VALUE = "", i = "caption")
}

vec_arith.ggmulti <- function(op, x, y, ...) {
  UseMethod("vec_arith.ggmulti", y)
}
vec_arith.ggmulti.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
vec_arith.ggmulti.gg <- function(op, x, y, ...) {
  vctrs::vec_arith_base(op, x, y)
}
vec_arith.ggmulti.labels <- function(op, x, y, ...) {
  vctrs::vec_arith_base(op, x, y)
}

#' @export
ggplot.ggmulti <- function(data = NULL, mapping = aes(), ..., environment = parent.frame(), fig_suffix = "\n\n") {
  for (i in seq_along(data)) {
    current_fig <- vctrs::field(data[[i]], "figure")[[1]]
    if (length(current_fig) == 0 || identical(NA, current_fig)) {
      warning("No figure for input index ", i)
    } else {
      print(current_fig)
      cat(fig_suffix)
    }
  }
}

#' @export
print.ggmulti <- function(x, ...) {
  ggplot.ggmulti(data = x, ...)
}
