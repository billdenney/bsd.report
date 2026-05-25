# Extract Log-Likelihood

See [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) for use

## Usage

``` r
# S3 method for class 'xpose_data'
logLik(object, ...)

# S3 method for class '`NULL`'
logLik(object, ...)

# S3 method for class 'logical'
logLik(object, ...)

# S3 method for class '`try-error`'
logLik(object, ...)
```

## Arguments

- object:

  any object from which a log-likelihood value, or a contribution to a
  log-likelihood value, can be extracted.

- ...:

  some methods for this generic function require additional arguments.

## Value

a logLik object

## Functions

- `` logLik(`NULL`) ``: logLik for NULL returns an NA object

- `logLik(logical)`: logLik for NA returns an NA object

- `` logLik(`try-error`) ``: logLik for try-error returns an NA object

## See also

[`NA_logLik_`](https://billdenney.github.io/bsd.report/reference/NA_logLik_.md)
