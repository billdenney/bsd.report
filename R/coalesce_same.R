#' Replace missing values with non-missing while confirming that non-missing
#' values are the same.
#' 
#' @param ... Values to fill in and compare
#' @return The value filled in
#' @seealso `dplyr::coalesce`
#' @export
coalesce_same <- function(...)
  UseMethod("coalesce_same")

#' @rdname coalesce_same
#' @export
coalesce_same.default <- function(...) {
  args <- list(...)
  if (length(args) == 1) {
    warning("`coalesce_same()` is typically called with >1 argument.")
  }
  ret <- args[[1]]
  for (idx in seq_len(length(args) - 1) + 1) {
    if (is.null(args[[idx]])) {
      # do nothing
    } else {
      if (!(length(args[[idx]]) %in% c(1, length(ret)))) {
        stop("Argument ", idx, " must have length 1 or the same length as the first argument.")
      } else if (
        !identical(class(ret), class(args[[idx]])) &
        # Special case for numeric becuase multiple numeric classes may be
        # acceptable to combine.
        !(is.numeric(ret) & is.numeric(args[[idx]]))) {
        stop("Argument ", idx, " must be a ", class(ret)[1], ", not a ", class(args[[idx]])[1], ".")
      }
      current_arg <-
        if (length(args[[idx]]) == 1) {
          rep(args[[idx]], length(ret))
        } else {
          args[[idx]]
        }
      mask_overlap <- !is.na(ret) & !is.na(current_arg)
      if (any(ret[mask_overlap] != current_arg[mask_overlap])) {
        stop("Some items in argument ", idx, " overlap with prior values, but the value is not the same.")
      }
      mask_new <- is.na(ret) & !is.na(current_arg)
      ret[mask_new] <- current_arg[mask_new]
    }
  }
  ret
}

#' @rdname coalesce_same
#' @details For data.frames, combines columns that match names from the first
#'   data.frame (extra columns from subsequent data.frames are ignored).
#' @export
coalesce_same.data.frame <- function(...) {
  args <- list(...)
  ret <- args[[1]]
  if (length(args) == 1) {
    warning("`coalesce_same()` is typically called with >1 argument.")
  }
  for (nm in names(ret)) {
    current_col_values <- list(ret[[nm]])
    for (idx in seq_len(length(args) - 1) + 1) {
      current_arg <- args[[idx]]
      if (!is.data.frame(current_arg)) {
        stop("Argument ", idx, " must be a data.frame since the first argument is a data.frame")
      }
      current_col_values <-
        if (nm %in% names(current_arg)) {
          append(current_col_values, list(current_arg[[nm]]))
        } else {
          append(current_col_values, list(NULL))
        }
    }
    ret[[nm]] <- do.call(coalesce_same, current_col_values)
  }
  ret
}
