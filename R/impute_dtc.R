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
  ret <-
    ret_prep %>%
    dplyr::group_by(STUDYID, USUBJID, NTSFD) %>%
    # If there is only one date/time for an NTSFD, use that.
    dplyr::mutate(
      # single_date and single_time only apply to scheduled measurements.
      # Unscheduled measurements are not imputed with those assumptions.
      single_date=!any(is.na(NTSFD)) & (length(unique(na.omit(DATE_PART))) == 1),
      single_time=!any(is.na(NTSFD)) & (length(unique(na.omit(TIME_PART))) == 1),
      current_impute=is.na(DATE_PART) & is.na(TIME_PART) & single_date & single_time,
      DATE_PART=
        dplyr::case_when(
          current_impute~c(unique(na.omit(DATE_PART)), NA_character_)[1],
          TRUE~DATE_PART
        ),
      TIME_PART=
        dplyr::case_when(
          current_impute~c(unique(na.omit(TIME_PART)), NA_character_)[1],
          TRUE~TIME_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Single date/time for the nominal time", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    # Will not impute multiple dates within the same NTSFD (may be a incorrect
    # if an event occurs near midnight)
    dplyr::mutate(
      current_impute=is.na(DATE_PART) & single_date,
      DATE_PART=
        dplyr::case_when(
          current_impute == 1~c(unique(na.omit(DATE_PART)), NA_character_)[1],
          TRUE~DATE_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Single date for the nominal time", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    # Check to see if only a single time value exists within an interval, and if
    # so, use that time.
    dplyr::mutate(
      current_impute=is.na(TIME_PART) & single_date & single_time,
      TIME_PART=
        dplyr::case_when(
          current_impute~c(unique(na.omit(TIME_PART)), NA_character_)[1],
          TRUE~TIME_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Single time measurment observed for a nominal time", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    # If multiple times occur at the same NTSFD but all on the same date, take the median.
    dplyr::mutate(
      current_impute=is.na(TIME_PART) & any(!is.na(TIME_PART)) & single_date,
      TIME_PART=
        dplyr::case_when(
          current_impute~median_character(TIME_PART, na.rm=TRUE),
          TRUE~TIME_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Median time within the observed nominal times", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ADTC_IMPUTED=
        dplyr::case_when(
          is.na(DATE_PART) | is.na(TIME_PART)~NA_character_,
          TRUE~paste(DATE_PART, TIME_PART, sep="T")
        )
    ) %>%
    dplyr::select(-DATE_PART, -TIME_PART, -current_impute, -single_date, -single_time)
  ret
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
      current_impute=!is.na(NTOD) & is.na(TIME_PART) & any(!is.na(TIME_PART)),
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
  if (any(c("DATE_PART", "TIME_PART", "current_impute") %in% names(data))) {
    stop("`data` cannot have columns named 'DATE_PART', 'TIME_PART', or 'current_impute' as those are used internally.")
  }
  ret_prep <- data
  if (!("ADTC_IMPUTED" %in% names(ret_prep))) {
    ret_prep$ADTC_IMPUTED <- NA_character_
  }
  if (!("ADTC_IMPUTE_METHOD" %in% names(ret_prep))) {
    ret_prep$ADTC_IMPUTE_METHOD <- NA_character_
  }
  ret_prep %>%
    # Separate the date and time
    tidyr::extract(
      col="ADTC",
      into=c("DATE_PART", "TIME_PART"),
      regex=
        paste0(
          "^",
          "([0-9]{4}-[0-9]{2}-[0-9]{2})",
          "(?:T",
          "(",
          "(?:UN|[0-9]{2}):", # hours
          "(?:UN|[0-9]{2})", # minutes
          "(?::(?:UN|[0-9]{2}))?", # maybe seconds
          ")",
          ")?$"
        ),
      remove=FALSE,
      convert=FALSE
    ) %>%
    dplyr::mutate(
      # Set unknown times to missing
      TIME_PART=
        dplyr::case_when(
          grepl(x=TIME_PART, pattern="UN")~NA_character_,
          TIME_PART %in% ""~NA_character_,
          TRUE~TIME_PART
        )
    ) %>%
    # Capture observed date/times
    dplyr::mutate(
      current_impute=!is.na(DATE_PART) & !is.na(TIME_PART) & is.na(ADTC_IMPUTE_METHOD),
      ADTC_IMPUTE_METHOD=
        case_when(
          current_impute~paste_missing(ADTC_IMPUTE_METHOD, "Observed date and time", sep="; "),
          TRUE~ADTC_IMPUTE_METHOD
        )
    ) %>%
    select(-current_impute)
}

utils::globalVariables(
  c(
    "ADTC_IMPUTE_METHOD", "current_impute", "DATE_PART", "NTOD", "NTSFD",
    "single_date", "single_time",
    "STUDYID", "TIME_PART", "USUBJID"
  )
)
