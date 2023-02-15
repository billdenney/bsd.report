#' @export
new_ggmulti <- function(figure = NULL, caption = character()) {
  if (length(caption) == 0) {
    caption <- character()
  } else {
    vctrs::vec_assert(caption, ptype = character(), size = 1)
  }
  if (is.null(figure)) {
    figure <- list()
  } else if (inherits(figure, "gg")) {
    figure <- list(figure)
  } else {
    stop("'figure' must either be a ggplot2 object or a ggmulti object")
  }
  vctrs::new_rcrd(list(figure = figure, caption = caption), class = "ggmulti")
}

#' @export
is_ggmulti <- function(x) {
  inherits(x, "ggmulti")
}

#' @export
format.ggmulti <- function(x, ...) {
  vctrs::field(x, "caption")
}

vec_ptype_abbr.ggmulti <- function(x, ...) {
  "ggmlt"
}

vec_ptype2.ggmulti.ggmulti <- function(x, y, ...) {
  new_ggmulti()
}

vec_ptype2.character.ggmulti <- function(x, y, ...) {
  character()
}

#' @export
vec_cast.ggmulti.character  <- function(x, to, ...) character()
#' @export
vec_cast.character.ggmulti  <- function(x, to, ...) character()

# print.ggmulti <- function(x, ...) {
#   plot.ggmulti(x, ...)
# }
#
plot.ggmulti <- function(x, y, ...) {
  stopifnot(missing(y))
  for (i in seq_along(x)) {
    current_fig <- vctrs::field(x[[i]], "figure")
    if (length(current_fig) == 1) {
      print(current_fig)
    } else {
      cat("NA\n")
    }
  }
}
