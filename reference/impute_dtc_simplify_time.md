# Confirm that the value looks like a time, and remove fully unknown times and zero seconds

A time is a 12- or 24-hour time possibly with "UN" as specified by the
CDISC SDTM standard. It may be of the format "" (empty string), \#:##,
\#:##:##, \##:##, \##:##:##, \#:##:UN, \##:##:UN, UN:UN, or UN:UN:UN
(where \# is a valid number only allowing hours from 00-23, and "UN"
signifies that the value is unknown).

## Usage

``` r
impute_dtc_simplify_time(x, drop_zero_seconds = FALSE)
```

## Arguments

- x:

  A character vector that looks like a time (see details)

- drop_zero_seconds:

  Drop :00 for seconds (keeping only hours and minutes)

## Value

A character vector that is simplified and is a time. Zero will be
prepended to single-digit hours.
