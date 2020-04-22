#' Replace values in a character string with their synonym
#' 
#' @param x The values to possibly replace
#' @param synonyms A named character vector where the names are the verbatim
#'   value and the values are the values to use for replacement.
#' @param ignore_case Should the synonyms be replaced case-insensitively?
#' @param ... Passed to other `replace_synonym()` methods.
#' @return `x` with preferred values instead of verbatim values.
#' @examples 
#' replace_synonym(
#'   c("A", "B", "C", "a"),
#'    c(A="apple", B="bear", C="cabbage")
#' )
#' replace_synonym(
#'   c("A", "B", "C", "a"),
#'   c(A="apple", B="bear", C="cabbage"),
#'   ignore_case=FALSE
#' )
#' replace_synonym(
#'   x=
#'     data.frame(
#'       A=rep(c("A", "B"), each=2),
#'       B=letters[1:4],
#'       stringsAsFactors=FALSE
#'     ),
#'     synonyms=
#'       data.frame(
#'         A="A",
#'         Column="B",
#'         Verbatim=c("a", "c"),
#'         Preferred=c("apple", "cherry"),
#'         stringsAsFactors=FALSE
#'       )
#' )
#' @export
#' @importFrom dplyr recode
replace_synonym <- function(x, synonyms, ignore_case=TRUE, ...)
  UseMethod("replace_synonym")

#' @rdname replace_synonym
#' @export
replace_synonym.character <- function(x, synonyms, ignore_case=TRUE, ...) {
  if (!is.character(x)) {
    stop("`x` must be a character vector")
  } else if (!is.character(synonyms)) {
    stop("`synonyms` must be a character vector.")
  } else if (is.null(names(synonyms))) {
    stop("`synonyms` must be named.")
  } else if (any(names(synonyms) %in% "")) {
    stop("All `synonyms` must be named, and no names may be blank.")
  } else if (any(duplicated(names(synonyms)))) {
    stop("All names of `synonyms` (verbatim values) must be unique.")
  }
  if (ignore_case) {
    names(synonyms) <- tolower(names(synonyms))
    # Only modify x if it will be replaced, otherwise, we do not want to just
    # return the lower-case version of `x`.
    mask_replaced <- tolower(x) %in% names(synonyms)
    x[mask_replaced] <- tolower(x[mask_replaced])
  }
  do.call(dplyr::recode, append(list(.x=x), as.list(synonyms)))
}

#' @rdname replace_synonym
#' @param replacement_column The column name in `synonyms` to find where in `x`
#'   to apply synonyms.
#' @param verbatim_column The column name in `synonyms` with verbatim terms.
#' @param preferred_column The column name in `synonyms` with preferred terms.
#' @importFrom dplyr left_join
#' @importFrom purrr is_scalar_character
#' @importFrom tidyr nest unnest
#' @export
replace_synonym.data.frame <- function(x, synonyms, ignore_case=TRUE, ...,
                                       replacement_column="Column",
                                       verbatim_column="Verbatim",
                                       preferred_column="Preferred") {
  if (!is.data.frame(synonyms)) {
    stop("`synonyms` must be a data.frame if `x` is a data.frame.")
  } else if (!purrr::is_scalar_character(replacement_column)) {
    stop("`replacement_column` must be a scalar character string")
  } else if (!purrr::is_scalar_character(verbatim_column)) {
    stop("`verbatim_column` must be a scalar character string")
  } else if (!purrr::is_scalar_character(preferred_column)) {
    stop("`preferred_column` must be a scalar character string")
  } else if (!is.character(synonyms[[replacement_column]])) {
    stop("`synonyms[[replacement_column]]` must be a character column.")
  } else if (!is.character(synonyms[[verbatim_column]])) {
    stop("`synonyms[[verbatim_column]]` must be a character column.")
  } else if (!is.character(synonyms[[preferred_column]])) {
    stop("`synonyms[[preferred_column]]` must be a character column.")
  }
  if (replacement_column %in% names(x)) {
    orig_replacement_column <- replacement_column
    replacement_column <- paste0(max(names(x)), "X")
    names(synonyms)[names(synonyms) %in% orig_replacement_column] <- replacement_column
  }
  synonym_cols <- c(replacement_column, verbatim_column, preferred_column)
  by_cols <- setdiff(names(synonyms), synonym_cols)
  if (length(by_cols)) {
    if (!all(by_cols %in% names(x))) {
      stop("All columns in `synonyms` other than the `replacement_column`, `verbatim_column`, and `preferred_column` must be names of `x`.")
    }

    synonym_nest <- tidyr::nest(synonyms, synonyms=synonym_cols)
    x_nest <-
      tidyr::nest(x, data=setdiff(names(x), by_cols))
    x_synonym <- dplyr::left_join(x_nest, synonym_nest, by=by_cols)
    x_synonym$preferred <-
      mapply(
        FUN=replace_synonym_single_data_frame,
        x=x_synonym$data,
        synonyms=x_synonym$synonyms,
        ignore_case=ignore_case,
        replacement_column=replacement_column,
        verbatim_column=verbatim_column,
        preferred_column=preferred_column,
        SIMPLIFY=FALSE
      )
    x_synonym$data <- x_synonym$synonyms <- NULL
    ret <- tidyr::unnest(data=x_synonym, cols="preferred")
  } else {
    ret <-
      replace_synonym_single_data_frame(
        x=x,
        synonyms=synonyms,
        ignore_case=ignore_case,
        replacement_column=replacement_column,
        verbatim_column=verbatim_column,
        preferred_column=preferred_column
      )
  }
  ret
}

# Helper function for replace_synonym.data.frame doing the work when a single
# data.frame is needed.
#' @importFrom stats setNames
replace_synonym_single_data_frame <- function(x, synonyms, ignore_case=TRUE,
                                              replacement_column="Column",
                                              verbatim_column="Verbatim",
                                              preferred_column="Preferred") {
  if (!is.null(synonyms)) {
    col_to_replace <- unique(synonyms[[replacement_column]])
    for (current_col in col_to_replace) {
      mask_synonym <- synonyms[[replacement_column]] %in% current_col
      x[[current_col]] <-
        replace_synonym.character(
          x=x[[current_col]],
          synonyms=
            stats::setNames(
              synonyms[[preferred_column]][mask_synonym],
              nm=synonyms[[verbatim_column]][mask_synonym]
            ),
          ignore_case=ignore_case
        )
    }
  }
  x
}
