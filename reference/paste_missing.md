# Concatenate strings dropping missing values

Concatenate strings dropping missing values

## Usage

``` r
paste_missing(
  ...,
  sep = " ",
  collapse = NULL,
  missing_values = NA,
  paste_last = FALSE
)
```

## Arguments

- ..., sep, collapse:

  See [`?paste`](https://rdrr.io/r/base/paste.html)

- missing_values:

  Values considered missing to be ignored in pasting.

- paste_last:

  When all `...` arguments have been combined and only one remains,
  should `paste` be called on that last argument? (Ignored if `collapse`
  is not `NULL`.)

## Value

A character vector of pasted values.

## Details

If all values are missing, the value from the first argument is
preserved. `paste_last` affects the final output; the main difference is
that if `FALSE`, `NA_character_` values will be preserved, and if
`TRUE`, `NA_character_` values will be converted to "NA" (as is the case
with [`paste()`](https://rdrr.io/r/base/paste.html)).
