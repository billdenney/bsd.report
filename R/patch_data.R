#' Patch one data set using another (deprecated)
#'
#' @param ... Ignored
#' @export
patch_data <- function(...) {
  stop(
    "patch_data() is removed. Use dplyr::rows_patch() to replace only NA values, ",
    "or dplyr::rows_update() to replace all matched values."
  )
}
