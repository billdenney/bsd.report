# Run grepl on a vector of patterns.

Run grepl on a vector of patterns.

## Usage

``` r
grepl_multi_pattern(pattern, x, ...)
```

## Arguments

- pattern:

  One or more patterns (see grepl)

- x:

  A string vector

- ...:

  Passed to grepl

## Value

A boolean vector if any pattern is matched

## See also

[`gsub_multi_pattern`](https://billdenney.github.io/bsd.report/reference/gsub_multi_pattern.md)

## Examples

``` r
grepl_multi_pattern(pattern=c("A", "B", "C"), LETTERS)
#>  [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE
```
