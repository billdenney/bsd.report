# Convert from Unicode to ASCII

Convert from Unicode to ASCII

## Usage

``` r
unicode_to_ascii(x, ...)

# Default S3 method
unicode_to_ascii(x, verbose = FALSE, ...)

# S3 method for class 'character'
unicode_to_ascii(
  x,
  verbose = FALSE,
  pattern = c("μ", "µ"),
  replacement = c("u", "u"),
  general_conversion = TRUE,
  ...
)

# S3 method for class 'factor'
unicode_to_ascii(x, ...)

# S3 method for class 'logical'
unicode_to_ascii(x, verbose = FALSE, ...)

# S3 method for class 'data.frame'
unicode_to_ascii(x, verbose = FALSE, ...)

# S3 method for class 'list'
unicode_to_ascii(x, verbose = FALSE, ...)
```

## Arguments

- x:

  Object that may contain character strings for conversion.

- verbose:

  Provide messages about number of values changed.

- pattern, replacement, ...:

  Passed to
  [`stringi::stri_replace_all_fixed()`](https://rdrr.io/pkg/stringi/man/stri_replace.html)

- general_conversion:

  After the initial, specific conversions, run
  `stringi::stri_trans_general(x, id="Any-Latin;Greek-Latin;Latin-ASCII")`
  on the current strings.

## Value

An object of the same class as `x` with Unicode characters converted to
ASCII.
