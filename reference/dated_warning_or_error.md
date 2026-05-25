# Give a warning or error based on the current date

Give a warning or error based on the current date

## Usage

``` r
dated_warning_or_error(date, ...)
```

## Arguments

- date:

  The date cutoff

- ...:

  Passed to `stop` if todays date is \< `date` or `warning` otherwise.
