#' Create new columns in a data.frame with sprintf results
#' 
#' @param data the data to use for formatting
#' @param ... a named list of character vectors.  Names are new columns for
#'   \code{data}, and values are sent to \code{format} in
#'   \code{sprintf_data_frame_single}.
#' @return The data frame with columns added for the names of \code{...}.
#' @examples
#' sprintf_data_frame(
#'   data=mtcars,
#'   cyl_mpg=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
#'   disp_hp=c(disp="%g cu.in. displacement, ", hp="%g hp")
#' )
#' @export
sprintf_data_frame <- function(data, ...) {
  args <- list(...)
  stopifnot("... arguments must be named"=!is.null(names(args)))
  stopifnot("All ... arguments must be named"=!any(names(args) %in% ""))
  stopifnot("No names of ... may match names of data"=!any(names(args) %in% names(data)))
  for (current_nm in names(args)) {
    data[[current_nm]] <-
      sprintf_data_frame_single(
        data=data,
        format=args[[current_nm]]
      )
  }
  data
}

#' @describeIn sprintf_data_frame Generate a character vector based on sprintf
#'   input formats
#'
#' @param format A named character vector where the names are column names in
#'   \code{data} and the values are sprintf format strings for the column.
#' @return A character vector with one element per row of \code{data}.
#' @examples
#' sprintf_data_frame_single(
#'   data=mtcars,
#'   format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
#' )
#' @export
sprintf_data_frame_single <- function(data, format) {
  stopifnot("'format' must be named"=!is.null(names(format)))
  stopifnot("All elements of 'format' must be named"=!any(names(format) %in% ""))
  stopifnot("All names of 'format' must be column headers in 'data'"=all(names(format) %in% names(data)))
  do.call(
    sprintf,
    append(
      list(fmt=paste0(format, collapse="")),
      unname(data[, names(format), drop=FALSE])
    )
  )
}
