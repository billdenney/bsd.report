#' Patch one data set using another
#'
#' @param basedata The original (source) data
#' @param patchdata The updated data
#' @param byvars The variables to match between source and updated data (all
#'   other columns in the updated data are used to patch the source data).
#' @param ... Ignored
#' @param replace Values selected in \code{basedata} for replacement with values
#'   in \code{patchdata} (via \code{%in%}).  (If \code{NULL}, all values may be
#'   replaced.)
#' @param do_not_replace Values in \code{patchdata} not to use for replacement
#'   of values in \code{basedata}.  (If \code{NULL}, all values may be used for
#'   replacement.)
#' @param verbose Report on replacement count by column.
#' @return The \code{basedata} updated with values from \code{patchdata}
#' @export
#' @importFrom dplyr group_vars
patch_data <- function(basedata, patchdata, by=dplyr::group_vars(basedata), ...,
                       replace=NA, do_not_replace=NA, verbose=TRUE) {
  if (length(by) < 1) {
    stop("`by` must be provided with at least one column.")
  }
  patch_cols <- setdiff(names(patchdata), by)
  if (!all(by %in% names(patchdata))) {
    stop("All names in `by` must be present as columns of `patchdata`.")
  } else if (nrow(patchdata) != nrow(unique(patchdata[,by,drop=FALSE]))) {
    stop("`patchdata` must have 0 or 1 row for each group in basedata")
  } else if (length(missing_names <- setdiff(patch_cols,
                                             setdiff(names(basedata), by))) > 0) {
    warning("Some column names are in patchdata but not in basedata (new columns will be added): ",
            paste(missing_names, collapse=", "))
  }
  ret <- full_join(basedata, patchdata,
                   by=by,
                   suffix=c("", ".patch"))
  for (nm in patch_cols) {
    col_patch <- paste0(nm, ".patch")
    if (!is.null(do_not_replace)) {
      mask_do_not_replace <- ret[[col_patch]] %in% do_not_replace
    } else {
      mask_do_not_replace <- rep(FALSE, nrow(ret))
    }
    if (!is.null(replace)) {
      mask_replace <- ret[[nm]] %in% replace
    } else {
      mask_replace <- rep(TRUE, nrow(ret))
    }
    mask_replace <- mask_replace & !mask_do_not_replace
    ret[[nm]][mask_replace] <- ret[[col_patch]][mask_replace]
    ret[paste0(nm, ".patch")] <- NULL
    if (verbose) {
      message("Replaced ", sum(mask_replace), " values in column ", nm)
    }
  }
  ret
}
