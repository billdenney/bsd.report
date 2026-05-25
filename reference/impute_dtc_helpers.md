# Impute the date and time within a single study/subject/nominal time where there is only one date/time combo in the interval.

Impute the date and time within a single study/subject/nominal time
where there is only one date/time combo in the interval.

## Usage

``` r
impute_dtc_helper_single_datetime(date, time, ntime, method)

impute_dtc_helper_single_date(date, time, ntime, method)

impute_dtc_helper_median_time(date, time, ntime, method)

impute_dtc_helper_time_ntod(date, time, ntime, method)
```

## Arguments

- date, time:

  Date and time parts of the date/time (character)

- ntime:

  Nominal time (numeric)

- method:

  Existing imputation methods used

## Value

A tibble with columns for date, time, ntime, and method

## Functions

- `impute_dtc_helper_single_datetime()`: Impute for a single date/time
  within a scheduled nominal time

- `impute_dtc_helper_single_date()`: Impute for a single date within a
  scheduled nominal time

- `impute_dtc_helper_median_time()`: Impute for the median time within a
  scheduled nominal time if all happen on the same date

- `impute_dtc_helper_time_ntod()`: Impute for the median time within a
  scheduled nominal time of day
