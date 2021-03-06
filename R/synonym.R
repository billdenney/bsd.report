#' Replace values in a character string with their synonym
#' 
#' @param x The values to possibly replace
#' @param synonyms A named character vector where the names are the verbatim
#'   value and the values are the values to use for replacement.  When `x` is a
#'   data.frame, this may be a data.frame with columns named
#'   `replacement_column`, `verbatim_column`, and `preferred_column` or a list
#'   of such data.frames.
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
#' @family Text standardization
#' @family Synonyms
#' @export
replace_synonym <- function(x, synonyms, ignore_case=TRUE, ...) {
  UseMethod("replace_synonym")
}

#' @rdname replace_synonym
#' @importFrom dplyr recode
#' @export
replace_synonym.character <- function(x, synonyms, ignore_case=TRUE, ...) {
  if (!is.character(x)) {
    stop("`x` must be a character vector")
  } else if (!is.character(synonyms)) {
    stop("`synonyms` must be a character vector.")
  } else if (is.null(names(synonyms))) {
    stop("`synonyms` must be named.")
  } else if (any(duplicated(names(synonyms)))) {
    dupes <- unique(names(synonyms)[duplicated(names(synonyms))])
    stop(
      "All names of `synonyms` (verbatim values) must be unique. Duplicated values are: ",
      paste0("`", dupes, "`", collapse=", ")
    )
  }
  if (any("" %in% names(synonyms))) {
    # "" is not allowed for recode, make a placeholder value and use it
    max_value <-
      paste0(
        max(
          c(
            max(x, na.rm=TRUE),
            names(synonyms),
            synonyms
          ),
          na.rm=TRUE
        ),
        "X"
      )
    names(synonyms)[names(synonyms) %in% ""] <- max_value
    x[x %in% ""] <- max_value
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

#' Use a regular expression to choose the synonyms in a list to replace in a data.frame.
#' 
#' @inheritParams replace_synonym
#' @param pattern The regular expression to use to choose the names from
#'   `synonyms` to use for replacement.
#' @param pattern_ignore_case Should case be ignored for name matching in
#'   `synonyms`?
#' @param ... Passed to `replace_synonym()`
#' @return The output of `replace_synonym()` applied to a subset of the list,
#'   `synonyms`.
#' @family Synonyms
#' @export
replace_synonym_list <- function(x, synonyms,
                                 pattern="^synonym", pattern_ignore_case=TRUE, ...) {
  if (is.null(names(synonyms)) || "" %in% names(synonyms)) {
    stop("All `synonyms` must be named.")
  } else if (is.data.frame(synonyms) || !is.list(synonyms)) {
    stop("`synonyms` must be a list and not a data.frame (see `replace_synonym()` for using a data.frame).")
  }
  matched_names <-
    grep(
      x=names(synonyms),
      pattern=pattern,
      ignore.case=pattern_ignore_case,
      value=TRUE
    )
  if (length(matched_names)) {
    mask_has_column <-
      sapply(
        synonyms[matched_names],
        FUN=function(current_synonym) any(current_synonym$Column %in% names(x))
      )
    matched_names_with_col <- matched_names[mask_has_column]
    message(
      "Using the following names as synonyms for possible replacement: ",
      paste("`", matched_names_with_col, "`", sep="", collapse=", ")
    )
    ret <-
      replace_synonym(
        x,
        synonyms=
          synonyms[matched_names_with_col],
        ...
      )
  } else {
    message("No synonym data.frames found in the list for possible replacement.")
    ret <- x
  }
  ret
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
  if (!is.data.frame(synonyms) & is.list(synonyms)) {
    ret <- x
    for (current_idx in seq_along(synonyms)) {
      ret <-
        replace_synonym.data.frame(
          ret,
          synonyms=synonyms[[current_idx]],
          ignore_case=ignore_case, ...,
          replacement_column=replacement_column,
          verbatim_column=verbatim_column,
          preferred_column=preferred_column
        )
    }
    return(ret)
  }
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
    warning("`replacement_column` (", replacement_column, ") is in `names(x)`.  Renaming it in `synonyms`.  If it is intended to be used for row-matching, please change `replacement_column`.")
    orig_replacement_column <- replacement_column
    replacement_column <- paste0(max(names(x)), "X")
    names(synonyms)[names(synonyms) %in% orig_replacement_column] <- replacement_column
  }
  synonym_cols <- c(replacement_column, verbatim_column, preferred_column)
  by_cols <- setdiff(names(synonyms), synonym_cols)
  if (!all(by_cols %in% names(x))) {
    warning(
      "All columns in `synonyms` other than the `replacement_column`, ",
      "`verbatim_column`, and `preferred_column` must be names of `x`.\n",
      "No synonyms will be applied due to the following missing columns: ",
      paste0("`", setdiff(by_cols, names(x)), "`", collapse=", ")
    )
    ret <- x
  } else if (length(by_cols)) {
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

#' Correct the case of a vector to be in a preferred case
#' 
#' @param x An object to correct the case of
#' @param preferred A character vector of preferred values
#' @return `x` where values that match `tolower(x) == tolower(preferred)` are
#'   converted to the preferred value.
#' @family Text standardization
#' @examples
#' correct_case(c("ABC", "Abc", "aBc", "def"), "Abc")
#' @export
correct_case <- function(x, preferred) {
  UseMethod("correct_case")
}

#' @export
correct_case.character <- function(x, preferred) {
  if (!is.character(preferred)) {
    stop("`preferred` must be a character vector.")
  } else if (any(duplicated(tolower(preferred)))) {
    stop("All `preferred` values must be unique, case-insensitively.")
  }
  l_x <- tolower(x)
  l_pref <- tolower(preferred)
  mask_replace <- l_x %in% l_pref
  if (any(mask_replace)) {
    for (current_pref in preferred) {
      x[l_x %in% tolower(current_pref)] <- current_pref
    }
  }
  x
}

#' @export
correct_case.factor <- function(x, preferred) {
  levels(x) <- correct_case(levels(x), preferred)
  x
}
