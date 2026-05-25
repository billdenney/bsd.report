# Perform a cumulative sum resetting to zero whenever a `reset` value is found.

Perform a cumulative sum resetting to zero whenever a `reset` value is
found.

## Usage

``` r
cumsum_reset(x, reset = NA)
```

## Arguments

- x:

  The vector to sum over

- reset:

  The value when found in `x` to reset the sum to zero at. (Note that
  the value at `x %in% reset` is zero.

## Value

A vector the same length as `x`
