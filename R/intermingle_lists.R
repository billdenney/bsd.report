#' Intermingle elements from a set of lists
#' 
#' @param ... lists with the same length as each other.
#' @return A list with the length of the sum of the input lists.
#' @examples
#' intermingle_list(as.list(1:5), as.list(6:10))
#' @export
intermingle_list <- function(...) {
  args <- list(...)
  if (!all(sapply(X=args, FUN=is.list))) {
    stop("All arguments must be lists")
  }
  input_lengths <- length(args[[1]])
  if (!all(sapply(X=args, FUN=length) == input_lengths)) {
    stop("All lists must be the same length")
  }
  ret <- rep(list(NULL), length(args) * input_lengths)
  idx <- seq_len(input_lengths) - 1
  for (i in seq_along(args)) {
    current_idx <- (idx * length(args)) + i
    ret[current_idx] <- args[[i]]
  }
  ret
}
