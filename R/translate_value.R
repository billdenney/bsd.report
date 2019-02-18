#' Convert a value from something old to something new (using gsub)
#' 
#' @param x The object to translate
#' @param old,new The old and new character strings
#' @param ... Passed to gsub
#' @param fixed Passed to gsub
#' @param exclude_col A vector of columns to exclude from data.frame translation
#' @return The object with old converted to new
#' @export
translate_value <- function(x, old, new, ...) {
  UseMethod("translate_value")
}

#' @describeIn translate_value Convert all columns (unless excluded)
#' @export
#' @importFrom dplyr mutate_at
translate_value.data.frame <- function(x, old, new, ..., exclude_col=NULL) {
  dplyr::mutate_at(
    .tbl=x,
    .vars=setdiff(names(x), exclude_col),
    .funs=translate_value,
    old=old, new=new, ...
  )
}

#' @describeIn translate_value Convert the levels
#' @export
translate_value.factor <- function(x, old, new, ...) {
  levels(x) <- translate_value(levels(x), old, new, ...)
  x
}

#' @describeIn translate_value Use gsub.
#' @export
translate_value.character <- function(x, old, new, ..., fixed=TRUE) {
  gsub(pattern=old, replacement=new, x=x, fixed=fixed, ...)
}

#' @describeIn translate_value No translation done.
#' @export
translate_value.default <- function(x, old, new, ...) {
  x
}
