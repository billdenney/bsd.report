# Expand II and ADDL with NONMEM-like methods for a single subject

Expand II and ADDL with NONMEM-like methods for a single subject

## Usage

``` r
realize_addl_single(time, evid, addl = 0, ii = 0)
```

## Arguments

- time:

  Time of assessment

- evid:

  NONMEM-style event identifier; 1 is a dose; 4 is a reset and dose; all
  other rows are ignored.

- addl:

  The number of additional doses (0 indicates one dose will be
  administered)

- ii:

  The interdose-interval (time between doses)

## Value

A data.frame with rows expanded based on addl and ii. The final number
of rows will be `sum(evid c(1, 4)])`
