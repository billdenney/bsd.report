# Ensure that a vector has only a single value throughout.

Missing values are replaced with the single value, and if all values are
missing, the first value in `missing` is used throughout.

## Usage

``` r
single_value(x, missing = NA, info = NULL)
```

## Arguments

- x:

  The vector which should have a single value

- missing:

  The vector of values to consider missing in `x`

- info:

  If more than one value is found, append this to the warning or error
  to assist with determining the location of the issue.

## Value

`x` as the scalar single value found throughout (or an error if more
than one value is found).

## Examples

``` r
single_value(c(NA, 1))
#> [1] 1
```
