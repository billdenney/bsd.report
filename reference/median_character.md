# Return the median of a character vector

Return the median of a character vector

## Usage

``` r
median_character(x, na.rm = TRUE, tie = c("lower", "upper"), ...)
```

## Arguments

- x:

  an object for which a method has been defined, or a numeric vector
  containing the values whose median is to be computed.

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds.

- tie:

  Are ties broken using the upper or lower value?

- ...:

  Passed to \`sort()\`

## Value

The median value of the vector

## Details

As half-way between two values in a character vector is not defined, in
the case of a median that would be between two values, either the lower
or upper value is selected.
