# Find the columns that have more than one value

Find the columns that have more than one value

## Usage

``` r
interesting_cols(x, keep_anyway = character(0), ...)
```

## Arguments

- x:

  The object (typically a data.frame) with columns to check.

- keep_anyway:

  A character vector of columns to keep even if they only have a single
  value.

- ...:

  ignored (for now)

## Value

An object of the same class as `x` with boring columns taken out and the
duplicated values placed in an attribute called `"boring"`.

## Examples

``` r
interesting_cols(data.frame(A=1:2, B=2))
#>   A
#> 1 1
#> 2 2
attr(interesting_cols(data.frame(A=1:2, B=2)), "boring")
#>   B
#> 1 2
```
