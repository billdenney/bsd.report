# Determine duplicate elements (including the first duplicate)

Determine duplicate elements (including the first duplicate)

## Usage

``` r
duplicated_including_first(x, ..., fromLast = NULL)
```

## Arguments

- x:

  A vector, data.frame, an array, or NULL.

- ...:

  Passed to `duplicated`

- fromLast:

  Ignored (included as an argument to prevent passing to `duplicated`)

## Value

A vector the same length as the return from `duplicated` with the first
duplicated value also flagged rather than just values from the second to
the last (as is returned from `duplicated`).
