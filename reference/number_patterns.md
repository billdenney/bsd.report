# Regular expression patterns for numbers

Regular expression patterns for numbers

## Usage

``` r
number_patterns
```

## Details

- natural:

  positive integers without a sign

- nonnegative_integer_no_sign:

  only digits

- integer:

  an optional +- at the beginning followed by only digits (note that
  "-0" is valid)

- float_no_sign:

  digits, a decimal point, and more digits

- float_or_integer_relaxed:

  an optional +- followed by one of the following:

  - digits

  - digits then a decimal point

  - a decimal point then digits

  - digits then a decimal point then digits

- float_or_integer_strict:

  an optional +- followed by digits optionally followed by a decimal
  point and digits (starting or ending with a decimal point is not
  allowed).

- scientific_notation:

  Strict scientific notation made of the following, in order:

  - an optional +-

  - a coefficient that is zero (with an optional decimal point and
    optional additional zeros), a natural number, or a float that does
    not start with zero

  - one of "e", "E", "d", or "D"

  - a optional +-

  - integer

- scientific_notation_relaxed:

  Relaxed scientific notation (relaxing on the coefficient rules) made
  of the following, in order:

  - float_or_integer_relaxed

  - one of "e", "E", "d", or "D"

  - a optional +-

  - integer

- number_relaxed:

  Any number format above (based on scientific_notation_relaxed with the
  exponent as an optional component)

## Examples

``` r
grepl(pattern=number_patterns$scientific_notation_relaxed, "1e5")
#> [1] TRUE
grepl(pattern=number_patterns$scientific_notation_relaxed, "1e-5")
#> [1] TRUE
grepl(pattern=number_patterns$scientific_notation_relaxed, "1e.5")
#> [1] FALSE
```
