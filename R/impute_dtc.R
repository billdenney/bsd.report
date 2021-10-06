#' Impute dates and times when data are missing.
#' 
#' @details Dates and times will be imputed based on the following rules:
#' 
#' \itemize{
#' \item{If both date and time are observed, no the observed value will be used.}
#' \item{Data are assumed to be grouped by appropriate grouping factors within a
#'   nominal time so that all times may be at the same time, and data are
#'   assumed to be sorted in the order specified in the protocol.}
#' \item{If nominal time since first dose (NTSFD) is missing, no imputation will be
#'   performed (the measure is assumed to be unscheduled).}
#' \item{If dates differ within a nominal time measurement, no imputation will be
#'   performed (a data issue would appear to exist in that case).}
#' \item{If only one date exists within a nominal time measurement, missing dates
#'   will be assumed to match the observed date.}
#' \item{If one or more time exists within a nominal interval, all measurements
#'   in the interval will be assigned to the median of the times that exist.}
#' }
#'
#' Columns used in calculation are:
#' 
#' \itemize{
#' \item{ADTC: (the date and time) formatted as an ISO8601 datetime without the time
#'   zone (yyyy-mm-ddThh:mm:ss) where the entire time or the seconds parts are
#'   optional.}
#' \item{STUDYID, USUBJID, NTSFD: grouping variables for the study number, subject
#'   identifier, and nominal time since first dose.}
#' }
#'
#' @param data A data.frame or equivalent object with at least the columns
#'   defined in the details section.
#' @return `data` with the columns "ADTC_IMPUTE_METHOD" and "ADTC_IMPUTED"
#'   added.
#' @family Imputation
#' @family Date/time imputation
#' @export
#' @importFrom dplyr `%>%` case_when group_by mutate select ungroup
impute_dtc <- function(data) {
  ret_prep <- impute_dtc_separate(data)
  ret_grouped <- dplyr::grouped_df(ret_prep, c("STUDYID", "USUBJID", "NTSFD"))
  idx_group <- dplyr::group_indices(ret_grouped)
  for (current_idx in seq_len(dplyr::n_groups(ret_grouped))) {
    current_mask <- idx_group %in% current_idx
    # Impute a single date/time within a NTSFD
    tmp <-
      impute_dtc_helper_single_datetime(
        date=ret_grouped$DATE_PART[current_mask],
        time=ret_grouped$TIME_PART[current_mask],
        ntime=ret_grouped$NTSFD[current_mask],
        method=ret_grouped$ADTC_IMPUTE_METHOD[current_mask]
      )
    # Impute a single date within a NTSFD
    tmp <-
      impute_dtc_helper_single_date(
        date=tmp$date,
        time=tmp$time,
        ntime=tmp$ntime,
        method=tmp$method
      )
    # Impute a single or median time within a NTSFD
    tmp <-
      impute_dtc_helper_median_time(
        date=tmp$date,
        time=tmp$time,
        ntime=tmp$ntime,
        method=tmp$method
      )
    tmp <-
      impute_dtc_helper_time_ntod(
        date=tmp$date,
        time=tmp$time,
        ntime=tmp$ntime,
        method=tmp$method
      )
    ret_grouped$DATE_PART[current_mask] <- tmp$date
    ret_grouped$TIME_PART[current_mask] <- tmp$time
    ret_grouped$ADTC_IMPUTE_METHOD[current_mask] <- tmp$method
  }
  # Drop the "NTSFD" group
  ret_grouped <- dplyr::grouped_df(ret_grouped, c("STUDYID", "USUBJID"))
  idx_group <- dplyr::group_indices(ret_grouped)
  for (current_idx in seq_len(dplyr::n_groups(ret_grouped))) {
    current_mask <- idx_group %in% current_idx
    # Impute a single date/time within a NTSFD
    tmp <-
      impute_dtc_helper_time_ntod(
        date=ret_grouped$DATE_PART[current_mask],
        time=ret_grouped$TIME_PART[current_mask],
        ntime=ret_grouped$NTSFD[current_mask],
        method=ret_grouped$ADTC_IMPUTE_METHOD[current_mask]
      )
    ret_grouped$DATE_PART[current_mask] <- tmp$date
    ret_grouped$TIME_PART[current_mask] <- tmp$time
    ret_grouped$ADTC_IMPUTE_METHOD[current_mask] <- tmp$method
  }
  ret <- ungroup(ret_grouped)
  ret$ADTC_IMPUTED <-
    ifelse(
      is.na(ret$DATE_PART) | is.na(ret$TIME_PART),
      NA_character_,
      paste(ret$DATE_PART, ret$TIME_PART, sep="T")
    )
  # Set ADTC_IMPUTE_METHOD to NA when imputation was not performed.  For
  # example, when it was possible to impute either DATE_PART or TIME_PART but
  # not both.
  ret$ADTC_IMPUTE_METHOD[is.na(ret$ADTC_IMPUTED)] <- NA_character_
  # Drop the internal columns
  ret[, setdiff(names(ret), c("DATE_PART", "TIME_PART")), drop=FALSE]
}

#' Impute the date and time within a single study/subject/nominal time where
#' there is only one date/time combo in the interval.
#' 
#' @param date,time Date and time parts of the date/time (character)
#' @param ntime Nominal time (numeric)
#' @param method Existing imputation methods used
#' @return A tibble with columns for date, time, ntime, and method
#' @name impute_dtc_helpers
NULL

#' @describeIn impute_dtc_helpers Impute for a single date/time within a
#'   scheduled nominal time
impute_dtc_helper_single_datetime <- function(date, time, ntime, method) {
  if (any(is.na(ntime))) {
    # Do nothing for unscheduled measurements
  } else {
    u_date <- unique(na.omit(date))
    u_time <- unique(na.omit(time))
    single_date <- length(u_date) == 1
    single_time <- length(u_time) == 1
    current_impute <- is.na(date) & is.na(time) & single_date & single_time
    date <-
      ifelse(
        current_impute,
        u_date,
        date
      )
    time <-
      ifelse(
        current_impute,
        u_time,
        time
      )
    method <-
      ifelse(
        current_impute,
        paste_missing(method, "Single date/time for the nominal time", sep="; "),
        method
      )
  }
  tibble(date=date, time=time, ntime=ntime, method=method)
}

#' @describeIn impute_dtc_helpers Impute for a single date within a scheduled
#'   nominal time
impute_dtc_helper_single_date <- function(date, time, ntime, method) {
  if (any(is.na(ntime))) {
    # Do nothing for unscheduled measurements
  } else {
    u_date <- unique(na.omit(date))
    single_date <- length(u_date) == 1
    current_impute <- is.na(date) & single_date
    date <-
      ifelse(
        current_impute,
        u_date,
        date
      )
    method <-
      ifelse(
        current_impute,
        paste_missing(method, "Single date for the nominal time", sep="; "),
        method
      )
  }
  tibble(date=date, time=time, ntime=ntime, method=method)
}

#' @describeIn impute_dtc_helpers Impute for the median time within a scheduled
#'   nominal time if all happen on the same date
impute_dtc_helper_median_time <- function(date, time, ntime, method) {
  if (any(is.na(ntime))) {
    # Do nothing for unscheduled measurements
  } else {
    u_date <- unique(na.omit(date))
    u_time <- unique(na.omit(time))
    if (length(u_time) == 1) {
      method_u <- "Single time measurement observed for a nominal time"
    } else {
      method_u <- "Median time within the observed nominal times"
    }
    time_new <- median_character(u_time)
    single_date <- length(u_date) == 1
    current_impute <- is.na(time) & !is.na(time_new) & single_date
    time <-
      ifelse(
        current_impute,
        time_new,
        time
      )
    method <-
      ifelse(
        current_impute,
        paste_missing(method, method_u, sep="; "),
        method
      )
  }
  tibble(date=date, time=time, ntime=ntime, method=method)
}

#' @describeIn impute_dtc_helpers Impute for the median time within a scheduled
#'   nominal time of day
impute_dtc_helper_time_ntod <- function(date, time, ntime, method) {
  ntod <- ntime %% 24
  # Unique, non-NA nominal times of day
  u_ntod <- unique(na.omit(ntod))
  if (length(u_ntod) == 1) {
    u_time <- unique(na.omit(time))
    if (length(u_time) == 1) {
      method_u <- "Single time measurement observed for a nominal time of day"
    } else {
      method_u <- "Median time within the observed nominal time of day"
    }
    time_new <- median_character(u_time)
    current_impute <- is.na(time) & !is.na(time_new)
    time <-
      ifelse(
        current_impute,
        time_new,
        time
      )
    method <-
      ifelse(
        current_impute,
        paste_missing(method, method_u, sep="; "),
        method
      )
  } else if (length(u_ntod) > 1) {
    for (current_ntod in u_ntod) {
      current_mask <- ntod %in% current_ntod
      tmp <-
        impute_dtc_helper_time_ntod(
          date=date[current_mask],
          time=time[current_mask],
          ntime=ntime[current_mask],
          method=method[current_mask]
        )
      time[current_mask] <- tmp$time
      method[current_mask] <- tmp$method
    }
  }
  tibble(date=date, time=time, ntime=ntime, method=method)
}

#' @describeIn impute_dtc imputes based on the typical nominal time of day
#'   (NTOD) for a subject.
#' @param na_ntod What nominal time of day should unscheduled measurements be
#'   imputed as?  (Often \code{0} is selected, but missing is the default.)
#' @export
impute_dtc_ntod <- function(data, na_ntod=NA_real_) {
  ret_prep <- impute_dtc_separate(data)
  ret_prep$NTOD <-
    ifelse(
      is.na(ret_prep$NTSFD),
      na_ntod,
      ret_prep$NTSFD %% 24
    )
  ret <-
    ret_prep %>%
    dplyr::mutate(
      current_impute=is.na(NTSFD) & !is.na(NTOD),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Assumed nominal time of day for unscheduled measure", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    dplyr::group_by(STUDYID, USUBJID, NTOD) %>%
    dplyr::mutate(
      current_impute=!is.na(NTOD) & !is.na(DATE_PART) & is.na(TIME_PART) & any(!is.na(TIME_PART)),
      TIME_PART=
        case_when(
          current_impute~median_character(unique(TIME_PART), na.rm=TRUE),
          TRUE~TIME_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Median time within the nominal time of day for the subject", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      ADTC_IMPUTED=
        dplyr::case_when(
          is.na(DATE_PART) | is.na(TIME_PART)~NA_character_,
          TRUE~paste(DATE_PART, TIME_PART, sep="T")
        )
    ) %>%
    dplyr::select(-DATE_PART, -TIME_PART, -NTOD, -current_impute)
  ret
}

#' Separate the date and time parts for imputation
#' @inheritParams impute_dtc
#' @return A dataset with the DATE_PART and TIME_PART separated from the ADTC
#'   column.
#' @noRd
#' @importFrom tidyr extract
impute_dtc_separate <- function(data) {
  if (any(c("DATE_PART", "TIME_PART") %in% names(data))) {
    stop("`data` cannot have columns named 'DATE_PART' or 'TIME_PART' as those are used internally.")
  }
  ret_prep_cols <- data
  if (!("ADTC_IMPUTED" %in% names(ret_prep_cols))) {
    ret_prep_cols$ADTC_IMPUTED <- NA_character_
    use_impute_column <- FALSE
  } else {
    use_impute_column <- TRUE
  }
  if (!("ADTC_IMPUTE_METHOD" %in% names(ret_prep_cols))) {
    ret_prep_cols$ADTC_IMPUTE_METHOD <- NA_character_
  }
  ret_prep <- impute_dtc_extract(ret_prep_cols, impute_column="ADTC")
  if (use_impute_column) {
    ret_prep_impute <- impute_dtc_extract(ret_prep, impute_column="ADTC_IMPUTED")
    mask_use_date_part <- !is.na(ret_prep_impute$DATE_PART) & is.na(ret_prep$DATE_PART)
    mask_use_time_part <- !is.na(ret_prep_impute$TIME_PART) & is.na(ret_prep$TIME_PART)
    ret_prep$DATE_PART[mask_use_date_part] <- ret_prep_impute$DATE_PART[mask_use_date_part]
    ret_prep$TIME_PART[mask_use_time_part] <- ret_prep_impute$TIME_PART[mask_use_time_part]
  }
  ret_prep %>%
    # Capture observed date/times
    dplyr::mutate(
      current_impute=!is.na(DATE_PART) & !is.na(TIME_PART) & is.na(ADTC_IMPUTE_METHOD),
      ADTC_IMPUTE_METHOD=
        case_when(
          is.na(ADTC_IMPUTE_METHOD) & current_impute~"Observed date and time",
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    select(-current_impute)
}

impute_dtc_extract <- function(data, impute_column) {
  # Separate the date and time
  pattern_ymd_hms <-
    paste0(
      "^",
      # year-month-day
      "([0-9]{4}-[0-9]{2}-[0-9]{2})",
      # maybe time
      "(?:T",
      # if time, then require hours and minutes
      "(",
      # Flexibly allow 1- or 2-digit hours (to be confirmed valid and fixed as
      # 2-digit later)
      "(?:UN|[0-9]{1,2}):", # hours
      "(?:UN|[0-9]{2})", # minutes
      "(?::(?:UN|[0-9]{2}))?", # maybe seconds
      ")",
      ")?$"
    )
  ret <-
    data %>%
    tidyr::extract(
      col=impute_column,
      into=c("DATE_PART", "TIME_PART"),
      regex=pattern_ymd_hms,
      remove=FALSE,
      convert=FALSE
    ) %>%
    dplyr::mutate(
      TIME_PART=impute_dtc_simplify_time(TIME_PART)
    )
  ret
}

#' Confirm that the value looks like a time, and remove fully unknown times and
#' zero seconds
#' 
#' A time is a 12- or 24-hour time possibly with "UN" as specified by the CDISC
#' SDTM standard.  It may be of the format "" (empty string), #:##, #:##:##,
#' ##:##, ##:##:##, #:##:UN, ##:##:UN, UN:UN, or UN:UN:UN (where # is a valid
#' number only allowing hours from 00-23, and "UN" signifies that the value is
#' unknown).
#' 
#' @param x A character vector that looks like a time (see details)
#' @param drop_zero_seconds Drop :00 for seconds (keeping only hours and
#'   minutes)
#' @return A character vector that is simplified and is a time.  Zero will be
#'   prepended to single-digit hours.
impute_dtc_simplify_time <- function(x, drop_zero_seconds=FALSE) {
  if (all(is.na(x))) {
    return(rep(NA_character_, length(x)))
  }
  stopifnot("x must be a character"=is.character(x))
  x_trimmed <- trimws(x)
  x_drop_unknown <-
    ifelse(
      x_trimmed %in% c("", "UN:UN", "UN:UN:UN"),
      NA_character_,
      x_trimmed
    )
  # Drop :UN seconds
  pattern_unknown_seconds <- "^((?:[0-9]|[01][0-9]|2[0-3]):[0-9]{2}):UN$"
  x_drop_unknown_seconds <-
    gsub(
      x=x_drop_unknown,
      pattern=pattern_unknown_seconds,
      replacement="\\1"
    )
  # Confirm valid time formats
  pattern_maybe_seconds <- "^((?:[0-9]|[01][0-9]|2[0-3]):[0-9]{2})(?::[0-9]{2})?$"
  good_times <-
    grepl(x=x_drop_unknown_seconds, pattern=pattern_maybe_seconds) |
    is.na(x_drop_unknown_seconds)
  if (!all(good_times)) {
    invalid_times <- unique(x[!good_times])
    stop(
      "x must look like a time (see help).  Invalid values: ",
      paste0(
        '"', invalid_times[seq_len(min(length(invalid_times), 5))], '"',
        collapse=", "
      )
    )
  }
  # Ensure 2-digit hours
  pattern_single_digit_hour <- "^([0-9]):"
  x_two_digit_hour <-
    gsub(
      x=x_drop_unknown_seconds,
      pattern=pattern_single_digit_hour,
      replacement="0\\1:"
    )
  # drop :00 seconds
  if (drop_zero_seconds) {
    pattern_zero_seconds <- "^((?:[01][0-9]|2[0-3]):[0-9]{2}):00$"
    x_zero_seconds <-
      gsub(
        x=x_two_digit_hour,
        pattern=pattern_zero_seconds,
        replacement="\\1"
      )
  } else {
    x_zero_seconds <- x_two_digit_hour
  }
  x_zero_seconds
}

#' Impute actual time using nominal time when actual is unavailable
#' 
#' @details The rules used are the following, in order.
#' 
#' \itemize{
#'   \item{If \code{actual} is not missing, use it.}
#'   \item{If there is at least one non-missing \code{actual} for a given
#'     \code{nominal}, take the median \code{actual} from the \code{nominal}.}
#'   \item{If for a missing actual time, the actual time values are in order
#'     compared to the nominal time:}
#'   \itemize{
#'     \item{If both directions with nominal and actual times (before and after)
#'       are within 24 hours, interpolate between.}
#'     \item{If only one direction with nominal and actual times (before or
#'       after) is within 24 hours, extrapolate in that direction ignoring the
#'       other direction.}
#'   }
#' }
#' 
#' @param actual,nominal Actual and nominal times as numeric vectors
#' @return \code{actual} with missing values imputed, as feasible, based on
#'   nominal times.
#' @family Imputation
#' @family Date/time imputation
#' @export
#' @importFrom dplyr arrange case_when group_by mutate
#' @importFrom zoo na.locf
impute_time_act_nom <- function(actual, nominal) {
  stopifnot(length(actual) == length(nominal))
  ret <-
    data.frame(
      actual=actual,
      nominal=nominal,
      nominal_with_actual=nominal,
      imputed=actual,
      method=c("Observed actual", NA_character_)[1 + is.na(actual)],
      ROWID=seq_along(actual),
      stringsAsFactors=FALSE
    )
  ret$nominal_with_actual[is.na(actual)] <- NA_real_
  ret <-
    ret %>%
    dplyr::group_by(nominal) %>%
    dplyr::mutate(
      # If there are multiple measures at the same nominal time with actual time
      # measures, use the median actual.
      current_impute=is.na(imputed) & any(!is.na(actual)) & is.na(actual) & !is.na(nominal),
      imputed=
        dplyr::case_when(
          current_impute~median(actual, na.rm=TRUE),
          TRUE~imputed
        )
    ) %>%
    dplyr::ungroup()
  # Sort by nominal then actual time to confirm if the two are in order relative
  # to each other.
  ret <- ret[order(nominal, actual), ]
  if (is.unsorted(ret$actual, na.rm=TRUE)) {
    warning("Some 'actual' times are not in 'nominal' time order")
  }
  ret$prior_nominal <- zoo::na.locf(ret$nominal_with_actual, na.rm=FALSE)
  ret$next_nominal <- rev(zoo::na.locf(rev(ret$nominal_with_actual), na.rm=FALSE))
  ret$prior_actual <- zoo::na.locf(ret$actual, na.rm=FALSE)
  ret$next_actual <- rev(zoo::na.locf(rev(ret$actual), na.rm=FALSE))

  # Interpolate with <=24 hours between observations
  mask_ordered_actual_within_24hr_both_directions <-
    # not already imputed
    is.na(ret$method) &
    # the actual values are in sort order
    (ret$prior_actual <= ret$next_actual) &
    # The current nominal time is within 24 hours in both directions
    (ret$nominal - ret$prior_nominal) <= 24 &
    (ret$next_nominal - ret$nominal) <= 24
  mask_ordered_actual_within_24hr_both_directions[is.na(mask_ordered_actual_within_24hr_both_directions)] <- FALSE
  value_ordered_actual_within_24hr_both_directions <-
    (ret$next_actual - ret$prior_actual)/(ret$next_nominal - ret$prior_nominal)*(ret$nominal - ret$prior_nominal) + ret$prior_actual
  ret$imputed[mask_ordered_actual_within_24hr_both_directions] <-
    value_ordered_actual_within_24hr_both_directions[mask_ordered_actual_within_24hr_both_directions]
  ret$method[mask_ordered_actual_within_24hr_both_directions] <-
    "<24 hr between two observed points"

  # Extrapolate forward with <=24 hours between observations
  mask_ordered_actual_within_24hr_after <-
    # not already imputed
    is.na(ret$method) &
    # the actual values are in sort order
    (is.na(ret$next_actual) | (ret$prior_actual <= ret$next_actual)) &
    # The current nominal time is within 24 hours of the prior
    (ret$nominal - ret$prior_nominal) <= 24
  mask_ordered_actual_within_24hr_after[is.na(mask_ordered_actual_within_24hr_after)] <- FALSE
  value_ordered_actual_within_24hr_both_directions <-
    ret$prior_actual + (ret$nominal - ret$prior_nominal)
  ret$imputed[mask_ordered_actual_within_24hr_after] <-
    value_ordered_actual_within_24hr_both_directions[mask_ordered_actual_within_24hr_after]
  ret$method[mask_ordered_actual_within_24hr_after] <-
    "Extrapolate forward <24h"

  # Extrapolate backward with <=24 hours between observations
  mask_ordered_actual_within_24hr_before <-
    # not already imputed
    is.na(ret$method) &
    # the actual values are in sort order
    (is.na(ret$prior_actual) | ret$prior_actual <= ret$next_actual) &
    # The current nominal time is within 24 hours of the prior
    (ret$next_nominal - ret$nominal) <= 24
  mask_ordered_actual_within_24hr_before[is.na(mask_ordered_actual_within_24hr_before)] <- FALSE
  value_ordered_actual_within_24hr_both_directions <-
    ret$next_actual + (ret$nominal - ret$next_nominal)
  ret$imputed[mask_ordered_actual_within_24hr_before] <-
    value_ordered_actual_within_24hr_both_directions[mask_ordered_actual_within_24hr_before]
  ret$method[mask_ordered_actual_within_24hr_before] <-
    "Extrapolate backward <24h"

  # Reorder the data and return it with the imputation method
  as.data.frame(ret)[order(ret$ROWID), c("imputed", "method")]
}

utils::globalVariables(
  c(
    "ADTC_IMPUTE_METHOD", "current_impute", "DATE_PART", "imputed", "NTOD", "NTSFD",
    "single_date", "single_time",
    "STUDYID", "TIME_PART", "USUBJID"
  )
)
