# Bidirectional setdiff rather than the default unidirectional version

Bidirectional setdiff rather than the default unidirectional version

## Usage

``` r
setdiff_bidir(x, y)
```

## Arguments

- x, y:

  As in `setdiff`

## Value

A vector of the two items with setdiff run in both directions. Items are
identified from the original set by names starting with "x" and "y".

## Examples

``` r
setdiff_bidir(1:5, 3:8)
#> x1 x2 y1 y2 y3 
#>  1  2  6  7  8 
```
