#' Sequence generation on an expoential scale
#'
#' @details If `from` is zero, then it will be modified to be
#'   `to/(100*length.out)` and zero will be added to the `add_values` vector.
#'
#' @inheritParams base::seq
#' @param by Explicitly ignored
#' @param add_values Add more values to the sequence
#' @param ... Passed to `base::seq()`
#' @return A sorted sequence
#' @export
seq_exp <- function(from=1, to=1, length.out=NULL, by=NULL, add_values=double(length=0), ...) {
  stopifnot(from >= 0)
  if (from == 0) {
    from <- to/(length.out*100)
    add_values <- c(0, add_values)
  }
  unique(sort(c(
    add_values,
    exp(base::seq(from=log(from), to=log(to), length.out=length.out, ...))
  )))
}
