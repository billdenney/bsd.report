#' Perform join where the outcome of the join is verifed to match an expected
#' pattern.
#' 
#' @details
#' Options for `x_control` and `y_control` are below and may be combined:
#'
#' * `"any"`: Any outcome is acceptable; this overrides all other options.
#' * `"all"`: Each row from the input must appear in the output at least one
#'   time.
#' * `"unique"`: A row may appear in the output zero or one time.
#' * `"missing"`: At least one row must not match in the new dataset (the values
#'    must be missing).  This option is rarely used.
#' * `"nomissing"`: All rows must match in the new dataset (the values must not
#'   be missing).
#' 
#' The combination of `x_control=c("all", "unique", "nomissing")` (or
#' `y_control`) is a common need to confirm that all values are present exactly
#' one time and that there are no missing values.
#' 
#' @param x,y tbls to join
#' @param join_fun Any function that can combine x and y (called as
#'   `join_fun(x, y, ...)`).  Typically this will be one of `dplyr::left_join`,
#'   `dplyr::right_join`, etc.
#' @param x_control,y_control What outcome is expected from the `x`, and `y` tbls? Default is "any" (see details).
#' @param ... Passed to `join_fun()`
#' @return A joined tbl
#' @export
join_control <- function(x, y, join_fun, x_control="any", y_control="any", ...) {
  control_choices <- c("any", "all", "unique", "missing", "nomissing")
  x_control <-
    match.arg(
      x_control,
      choices=control_choices,
      several.ok=TRUE
    )
  y_control <-
    match.arg(
      y_control,
      choices=control_choices,
      several.ok=TRUE
    )
  if (all(c("missing", "nomissing") %in% x_control)) {
    stop("Both 'missing' and 'nomissing' may not be provided at the same time for `x_control`.")
  } else if (all(c("missing", "nomissing") %in% y_control)) {
    stop("Both 'missing' and 'nomissing' may not be provided at the same time for `y_control`.")
  }
  max_name <- max(c(names(x), names(y)))
  col_x_detect <- paste0(max_name, "x")
  col_y_detect <- paste0(max_name, "y")
  x[[col_x_detect]] <- seq_len(nrow(x))
  y[[col_y_detect]] <- seq_len(nrow(y))
  ret <- join_fun(x, y, ...)
  join_control_detect(
    x=ret,
    control=x_control,
    detect_column=x[, col_x_detect, drop=FALSE],
    msg_prefix="x"
  )
  join_control_detect(
    x=ret,
    control=y_control,
    detect_column=y[, col_y_detect, drop=FALSE],
    msg_prefix="y"
  )
  ret[, setdiff(names(ret), c(col_x_detect, col_y_detect)), drop=FALSE]
}

#' @importFrom stats na.omit
join_control_detect <- function(x, control, detect_column, msg_prefix) {
  # The na.omit() is because NA values are managed by "missing" and "nomissing"
  all_in <- all(detect_column[[1]] %in% stats::na.omit(x[[names(detect_column)]]))
  uniq_in <- !any(duplicated(x[[names(detect_column)]]))
  missing_in <- any(is.na(x[[names(detect_column)]]))
  nomissing_in <- !missing_in
  if ("any" %in% control) {
    # do nothing
  } else {
    if ("all" %in% control & !all_in) {
      stop("`", msg_prefix, "`: All rows were are not in the new dataset.")
    }
    if ("unique" %in% control & !uniq_in) {
      stop("`", msg_prefix, "`: Rows are not unique in the new dataset.")
    }
    if ("missing" %in% control & !missing_in) {
      stop("`", msg_prefix, "`: No rows are missing in the new dataset.")
    }
    if ("nomissing" %in% control & !nomissing_in) {
      stop("`", msg_prefix, "`: Rows are missing in the new dataset.")
    }
  }
  x
}

#' @describeIn join_control For the common task of many-to-one mapping, the
#'   helper function `join_many_to_one()` works.
#' @importFrom dplyr left_join
#' @export
join_many_to_one <- function(x, y) {
  join_control(
    x, y,
    join_fun=dplyr::left_join,
    x_control=c("all", "unique", "nomissing"),
    y_control="nomissing"
  )
}