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
    # Will not impute multiple dates within the same NTSFD (may be a incorrect
    # if an event occurs near midnight)
    dplyr::mutate(
      current_impute=length(unique(na.omit(DATE_PART))),
      DATE_PART=
        dplyr::case_when(
          !is.na(DATE_PART)~DATE_PART,
          current_impute == 1~c(unique(na.omit(DATE_PART)), NA_character_)[1],
          TRUE~DATE_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          !is.na(ADTC_IMPUTE_METHOD)~ADTC_IMPUTE_METHOD,
          (current_impute > 1) & is.na(ADTC_IMPUTE_METHOD)~
            "Multiple dates observed during the same nominal time, not imputing",
          (current_impute == 0) & is.na(ADTC_IMPUTE_METHOD)~
            "No dates observed during the nominal time, not imputing",
          TRUE~NA_character_
        )
    ) %>%
    # Check to see if only a single time value exists within an interval, and if
    # so, use that time.
    dplyr::mutate(
      current_impute=
        (length(unique(na.omit(TIME_PART))) == 1) &
        is.na(ADTC_IMPUTE_METHOD),
      TIME_PART=
        dplyr::case_when(
          !is.na(TIME_PART)~TIME_PART,
          current_impute~c(unique(na.omit(TIME_PART)), NA_character_)[1],
          TRUE~TIME_PART
        ),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          !is.na(ADTC_IMPUTE_METHOD)~ADTC_IMPUTE_METHOD,
          current_impute~
            "Single time measurment observed for a nominal time",
          TRUE~NA_character_
        )
    ) %>%
    # Check to see if additional imputation is required
    dplyr::mutate(
      TIME_PART=
        dplyr::case_when(
          !is.na(TIME_PART)~TIME_PART,
          is.na(ADTC_IMPUTE_METHOD)~median_character(TIME_PART, na.rm=TRUE),
        ),
      current_impute=is.na(ADTC_IMPUTE_METHOD) & !is.na(TIME_PART),
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          !is.na(ADTC_IMPUTE_METHOD)~ADTC_IMPUTE_METHOD,
          current_impute~"Median time within the observed nominal times"
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
    dplyr::select(-DATE_PART, -TIME_PART, -current_impute)
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
  data %>%
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
          TRUE~TIME_PART
        ),
      # Capture observed DTC and unknown DTC
      ADTC_IMPUTE_METHOD=
        dplyr::case_when(
          !is.na(DATE_PART) & !is.na(TIME_PART)~"Observed date and time",
          is.na(NTSFD)~"Unscheduled measurement (missing NTSFD)",
          TRUE~NA_character_
        )
    )
}

utils::globalVariables(
  c(
    "ADTC_IMPUTE_METHOD", "current_impute", "DATE_PART", "NTSFD", "STUDYID",
    "TIME_PART", "USUBJID"
  )
)
