# Return the minimum of the differences of x for the first value after the choices.

Return the minimum of the differences of x for the first value after the
choices.

## Usage

``` r
mindiff_after(x, choices, include_zero = TRUE, none = c("negative", "na"))
```

## Arguments

- x:

  The values to select choices for.

- choices:

  The choices from which to select the minimum value.

- include_zero:

  Should choices only be selected if they are strictly after or should
  zero be allowed?

- none:

  What do you do if `x < min(choices)`? Return a `"negative"` value or
  return `NA`?

## Value

A vector the same length as `x` with the minimum difference between `x`
and any value of `choices`. If `choices` has length 0, then return a
vector of `NA` the same length as `x` (with a warning).

## See also

[`mindiff`](https://billdenney.github.io/bsd.report/reference/mindiff.md)
