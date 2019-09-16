#' Set a single baseline value given input data.
#'
#' @details
#' Possible baseline values are selected when:
#' \itemize{
#'   \item{`x` is not `NA`}
#'   \item{`time` is not `NA`}
#'   \item{`min_bl_time <= time`}
#'   \item{`time <= max_bl_time`}
#' }
#' 
#' If no value is possible, NA is returned (of the same class as `x`).  Within
#' the possible baseline values, the values with the maximum time are selected,
#' and all selected values are summarized with `summaryfun(selected_values, ...)`
#'
#' @param x A vector of observations
#' @param time A vector of times when the observations occurred
#' @param min_bl_time,max_bl_time The minum and maximum possible times for
#'   baseline measurements.
#' @param summaryfun What functions should summarize or modify the baseline
#'   value(s)?
#' @param ... Passed to `summaryfun()`
#' @return A scalar value of the baseline measurement
#' @export
set_baseline <- function(x, time, min_bl_time=-Inf, max_bl_time=0, summaryfun=mean, ...) {
  stopifnot(length(x) == length(time))
  stopifnot(is.numeric(time))
  stopifnot(length(min_bl_time) == 1)
  stopifnot(length(max_bl_time) == 1)
  stopifnot(is.numeric(min_bl_time) & !is.factor(min_bl_time))
  stopifnot(is.numeric(max_bl_time) & !is.factor(max_bl_time))
  stopifnot(!is.na(min_bl_time))
  stopifnot(!is.na(max_bl_time))
  mask_usable_values <-
    !is.na(x) &
    !is.na(time) &
    min_bl_time <= time &
    time <= max_bl_time
  if (any(mask_usable_values)) {
    mask_best_time <- time == max(time[mask_usable_values])
    # In case there are multiple measurements at the given time.
    mask_final_values <- mask_usable_values & mask_best_time
    summaryfun(x[mask_final_values], ...)
  } else {
    # Ensure that the output NA is the same class as `x`
    c(x[mask_usable_values], NA)
  }
}
