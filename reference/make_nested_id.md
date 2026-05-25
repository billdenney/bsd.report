# Generate a numeric identifier attempting to be stable to changes within groups.

Generate a numeric identifier attempting to be stable to changes within
groups.

## Usage

``` r
make_nested_id(..., outer = TRUE)
```

## Arguments

- ...:

  Either a single data.frame (or similar object) or something that can
  be coerced to a data.frame.

- outer:

  Don't touch this (it controls recursion).

## Value

An integer vector with unique identifiers that are nested with numbers
indicating group identifiers in a way that is stable to changes within
other groups.

## Details

The objective of this function is to make identifiers that are generally
insensitive to changes within other groups. The only time that
identifiers will change for groups that do not have changes is if one
level of identifiers has a new order of magnitude (e.g. if the number of
unique values within one of the groups goes from 99 to 100).

Values are converted to integers with \`as.integer(factor(x))\`. To gain
more control over the order, set values to factors prior to calling this
function.
