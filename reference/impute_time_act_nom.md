# Impute actual time using nominal time when actual is unavailable

Impute actual time using nominal time when actual is unavailable

## Usage

``` r
impute_time_act_nom(actual, nominal)
```

## Arguments

- actual, nominal:

  Actual and nominal times as numeric vectors

## Value

`actual` with missing values imputed, as feasible, based on nominal
times.

## Details

The rules used are the following, in order.

- If `actual` is not missing, use it.

- If there is at least one non-missing `actual` for a given `nominal`,
  take the median `actual` from the `nominal`.

- If for a missing actual time, the actual time values are in order
  compared to the nominal time:

  - If both directions with nominal and actual times (before and after)
    are within 24 hours, interpolate between.

  - If only one direction with nominal and actual times (before or
    after) is within 24 hours, extrapolate in that direction ignoring
    the other direction.

## See also

Other Imputation:
[`impute_dtc()`](https://billdenney.github.io/bsd.report/reference/impute_dtc.md)

Other Date/time imputation:
[`impute_dtc()`](https://billdenney.github.io/bsd.report/reference/impute_dtc.md)
