# Filter if it works, otherwise, do nothing

Filter if it works, otherwise, do nothing

## Usage

``` r
filter_maybe(.data, ..., .ignore_errors = TRUE)
```

## Arguments

- .data:

  The data to potentially filter

- ...:

  Arguments (see dplyr::filter)

- .ignore_errors:

  (always true)

## Value

Either NULL if an error or the filtered results.

## See also

[`filter_list`](https://billdenney.github.io/bsd.report/reference/filter_list.md)
