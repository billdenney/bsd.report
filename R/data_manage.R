#' Get a vector of column names expected for a dataset.
#' 
#' @details The input `data` must have columns named "Column Type" and "Column
#'   Name".  The "Column Type" defines arbitrary strings to be matched to subset
#'   for the "Column Name"s of interest.  The "Column Name" are the names
#'   themselves.
#'   
#' If not all "Column Type" values are in `data[["Column Type"]]`, an error will
#' be raised.
#'   
#' @param data A data.frame or similar object
#' @param coltype A vector of values to match in the "Column Type" column of
#'   `data` or `NULL` to match all values.
#' @return A vector of the subset of "Column Name" values that are in rows of
#'   "Column Type".
#' @family Data Management
#' @export
#' @importFrom dplyr `%>%` arrange filter mutate
#' @importFrom forcats fct_inorder
get_data_manage_standard_cols <- function(data, coltype) {
  ret_prep <-
    (if (is.null(coltype)) {
      data
    } else {
      data %>%
        dplyr::filter(`Column Type` %in% coltype)
    }) %>%
    dplyr::mutate(
      `Column Type`=forcats::fct_inorder(`Column Type`, ordered=TRUE)
    ) %>%
    dplyr::arrange(`Column Type`)
  # verify that there were no typos in coltype
  missing_coltype <- setdiff(coltype, ret_prep[["Column Type"]])
  if (length(missing_coltype)) {
    stop(
      "The following `Column Type` values were not found: ",
      paste0("`", missing_coltype, "`", collapse=", ")
    )
  }
  ret_prep[["Column Name"]]
}

#' Verify that a data.frame has the expected columns present
#' 
#' @details Either more or fewer columns are an error.
#' 
#' @param data A data.frame or similar object
#' @param cols A character vector of expected column names
#' @return `data` where the columns are ordered according to the order in
#'   `cols`.
#' @family Data Management
#' @export
check_expected_cols <- function(data, cols) {
  extra_cols <- setdiff(names(data), cols)
  missing_cols <- setdiff(cols, names(data))
  error_message_extra <-
    paste0(
      "The following extra columns are present: ",
      paste(extra_cols, collapse=", ")
    )
  error_message_missing <-
    paste0(
      "The following columns are missing: ",
      paste(missing_cols, collapse=", ")
    )
  if (length(extra_cols) & length(missing_cols) > 0) {
    stop(
      "The following column issues are present:\n",
      error_message_extra, "\n",
      error_message_missing
    )
  } else if (length(extra_cols)) {
    stop(
      "The following column issues are present:\n",
      error_message_extra
    )
  } else if (length(missing_cols)) {
    stop(
      "The following column issues are present:\n",
      error_message_missing
    )
  }
  # Provide the data output in the 
  data[, cols, drop=FALSE]
}

#' Output a data.frame with numeric columns on the left.
#' 
#' @param x A data.frame or similar object
#' @family Data Management
#' @export
nonmem_column_order <- function(x) {
  numeric_cols <- names(x)[sapply(X=x, FUN=is.numeric)]
  other_cols <- setdiff(names(x), numeric_cols)
  x[, c(numeric_cols, other_cols), drop=FALSE]
}
