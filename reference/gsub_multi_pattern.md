# Substitute from a vector of patterns with a single replacement

Substitute from a vector of patterns with a single replacement

## Usage

``` r
gsub_multi_pattern(x, patterns, replacement, ..., verbose = FALSE)
```

## Arguments

- x, replacement:

  See `gsub`

- patterns:

  A vector of patterns as used individually in `gsub` and `grepl`

- ...:

  Passed to `gsub` and `grepl`

- verbose:

  Signal messages with the count of values that matched each pattern or
  no pattern.

## Value

A vector of `NA_character_` when no match occurs and the replaced value
when a match occurs.

## See also

[`grepl_multi_pattern`](https://billdenney.github.io/bsd.report/reference/grepl_multi_pattern.md)
