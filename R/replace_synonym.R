#' Replace values in a character string with their synonym
#' 
#' @param x The values to possibly replace
#' @param synonyms A named character vector where the names are the verbatim value and the values are the values to use for replacement.
#' @param ignore_case Should the synonyms be replaced case-insensitively?
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
#' @export
#' @importFrom dplyr recode
replace_synonym <- function(x, synonyms, ignore_case=TRUE) {
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
