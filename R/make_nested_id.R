#' Generate a numeric identifier attempting to be stable to changes within
#' groups.
#' 
#' @details The objective of this function is to make identifiers that are
#'   generally insensitive to changes within other groups.  The only time that
#'   identifiers will change for groups that do not have changes is if one level
#'   of identifiers has a new order of magnitude (e.g. if the number of unique
#'   values within one of the groups goes from 99 to 100).
#'   
#'   Values are converted to integers with `as.integer(factor(x))`.  To gain
#'   more control over the order, set values to factors prior to calling this
#'   function.
#' 
#' @param ... Either a single data.frame (or similar object) or something that
#'   can be coerced to a data.frame.
#' @param outer Don't touch this (it controls recursion).
#' @return An integer vector with unique identifiers that are nested with
#'   numbers indicating group identifiers in a way that is stable to changes
#'   within other groups.
#' @importFrom dplyr group_by_at tibble
#' @importFrom purrr pmap
#' @importFrom tidyr nest unnest
#' @export
make_nested_id <- function(..., outer=TRUE) {
  args <- dplyr::tibble(...)
  ret <- args
  if (ncol(args) == 1) {
    ret[[1]] <- as.integer(factor(args[[1]]))
  } else {
    # Replace the inner groups with an integer
    ret <- tidyr::nest(dplyr::group_by_at(args, 1))
    # The following lines of code use numeric indices to ensure that name
    # clashes do not cause problems.
    ret[[2]] <- purrr::pmap(.l=list(ret[[2]]), .f=make_nested_id, outer=FALSE)
    # Replace the outer group with an integer.
    ret[1] <- make_nested_id(ret[1], outer=FALSE)
    ret <- tidyr::unnest(ret, cols=names(ret)[2])
  }
  if (outer) {
    # Generate a base-10 multiplier for each column
    column_multiplier <-
      10^rev(cumsum(rev(c(
        unname(sapply(
          ret,
          FUN=function(x) {
            ret <- ceiling(max(log10(x)))
            if (any(x >= (10^ret))) {
              # handle values of 10, 100, 1000, etc.
              ret <- ret + 1
            }
            ret
          }
        ))[-1],
        0
      ))))
    # Multiply each column by the value to get the digits in the correct place.
    ret_prep <- base::mapply(FUN="*", as.integer(column_multiplier), ret)
    # Take the sum of each row to get the identifier.
    ret <- base::rowSums(ret_prep)
  }
  ret
}
