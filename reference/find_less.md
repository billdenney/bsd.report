# Find the choice that is less than a value

Find the choice that is less than a value

## Usage

``` r
find_less(x, choices, include_same = TRUE, none = c("first", "na"))
```

## Arguments

- x:

  The vector of values to select choices for.

- choices:

  The choices that are to be selected from.

- include_same:

  Include if the choice is `<=` and not strictly `<`.

- none:

  What if the `x` value is less than (or less than or equal to if
  `include_same`) all the choices? Choose the `"first"` choice or insert
  an `NA` value?

## Value

A vector with values from `choices` that are less than (or equal to) `x`

## Details

`x` and `choices` must be comparable by the `<` or `<=` operators.
`choices` will be sorted to be in ascending order; sorting will remove
any `NA` values from choices.
