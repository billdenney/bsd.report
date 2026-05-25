# Impute dates and times when data are missing.

Impute dates and times when data are missing.

## Usage

``` r
impute_dtc(data)

impute_dtc_ntod(data, na_ntod = NA_real_)
```

## Arguments

- data:

  A data.frame or equivalent object with at least the columns defined in
  the details section.

- na_ntod:

  What nominal time of day should unscheduled measurements be imputed
  as? (Often `0` is selected, but missing is the default.)

## Value

\`data\` with the columns "ADTC_IMPUTE_METHOD" and "ADTC_IMPUTED" added.

## Details

Dates and times will be imputed based on the following rules:

- If both date and time are observed, no the observed value will be
  used.

- Data are assumed to be grouped by appropriate grouping factors within
  a nominal time so that all times may be at the same time, and data are
  assumed to be sorted in the order specified in the protocol.

- If nominal time since first dose (NTSFD) is missing, no imputation
  will be performed (the measure is assumed to be unscheduled).

- If dates differ within a nominal time measurement, no imputation will
  be performed (a data issue would appear to exist in that case).

- If only one date exists within a nominal time measurement, missing
  dates will be assumed to match the observed date.

- If one or more time exists within a nominal interval, all measurements
  in the interval will be assigned to the median of the times that
  exist.

Columns used in calculation are:

- ADTC: (the date and time) formatted as an ISO8601 datetime without the
  time zone (yyyy-mm-ddThh:mm:ss) where the entire time or the seconds
  parts are optional.

- STUDYID, USUBJID, NTSFD: grouping variables for the study number,
  subject identifier, and nominal time since first dose.

## Functions

- `impute_dtc_ntod()`: imputes based on the typical nominal time of day
  (NTOD) for a subject.

## See also

Other Imputation:
[`impute_time_act_nom()`](https://billdenney.github.io/bsd.report/reference/impute_time_act_nom.md)

Other Date/time imputation:
[`impute_time_act_nom()`](https://billdenney.github.io/bsd.report/reference/impute_time_act_nom.md)
