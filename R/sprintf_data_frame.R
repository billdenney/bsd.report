#' Create new columns in a data.frame with sprintf results
#' 
#' @param data the data to use for formatting
#' @param ... a named list of character vectors.  Names are new columns for
#'   \code{data}, and values are sent to \code{format} in
#'   \code{sprintf_data_frame_single}.
#' @param factor_out_if_factor_in If any of the input columns are factors, make
#'   the output column a factor in the same order as the input column factors
#' @param ordered If \code{factor_out_if_factor_in} converts the output to a
#'   factor, pass to \code{base::factor}.  If \code{NULL}, then it is set to
#'   \code{TRUE} if any of the input columns are ordered factors.
#' @return The data frame with columns added for the names of \code{...}.
#' @examples
#' sprintf_data_frame(
#'   data=mtcars,
#'   cyl_mpg=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
#'   disp_hp=c(disp="%g cu.in. displacement, ", hp="%g hp")
#' )
#' @export
sprintf_data_frame <- function(data, ..., factor_out_if_factor_in=TRUE, ordered=NULL) {
  args <- list(...)
  stopifnot("... arguments must be named"=!is.null(names(args)))
  stopifnot("All ... arguments must be named"=!any(names(args) %in% ""))
  stopifnot("No names of ... may match names of data"=!any(names(args) %in% names(data)))
  for (current_nm in names(args)) {
    data[[current_nm]] <-
      sprintf_data_frame_single(
        data=data,
        format=args[[current_nm]],
        factor_out_if_factor_in=factor_out_if_factor_in,
        ordered=ordered
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
sprintf_data_frame_single <- function(data, format, factor_out_if_factor_in=TRUE, ordered=NULL) {
  stopifnot("'format' must be named"=!is.null(names(format)))
  stopifnot("All elements of 'format' must be named"=!any(names(format) %in% ""))
  stopifnot("All names of 'format' must be column headers in 'data'"=all(names(format) %in% names(data)))
  ret <-
    do.call(
      sprintf,
      append(
        list(fmt=paste0(format, collapse="")),
        unname(data[, names(format), drop=FALSE])
      )
    )
  if (factor_out_if_factor_in) {
    d_inputs <- unique(data[, names(format), drop=FALSE])
    if (any(sapply(X=d_inputs, FUN=is.factor))) {
      if (is.null(ordered)) {
        ordered <- any(sapply(X=d_inputs, FUN=is.ordered))
      }
      d_inputs$rowid <- seq_len(nrow(d_inputs))
      d_inputs <- dplyr::arrange(.data=d_inputs, dplyr::across(.cols=everything()))
      ret <- base::factor(ret, levels=unique(ret)[d_inputs$rowid], ordered=ordered)
    }
  }
  ret
}
