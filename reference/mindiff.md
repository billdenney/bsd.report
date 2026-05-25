# Find the minimum difference between a vector and a set of choices.

Find the minimum difference between a vector and a set of choices.

## Usage

``` r
mindiff(x, choices, tie = c("first", "last", "median-first", "median-last"))
```

## Arguments

- x:

  The vector of values to adjust

- choices:

  The vector of values which should be matched

- tie:

  How should a tie (two choices are equidistant) be managed? (See
  details.)

## Value

The vector of `x` replaced by the closest values from the `choices`
vector.

## See also

[`mindiff_after`](https://billdenney.github.io/bsd.report/reference/mindiff_after.md)
