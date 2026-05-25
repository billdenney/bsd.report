# Perform an operation on a vector that may be empty or all NA with controlled output.

Perform an operation on a vector that may be empty or all NA with
controlled output.

## Usage

``` r
fun_no_na(x, FUN, zero_len = NULL)

max_no_na(x, zero_len = NA)

min_no_na(x, zero_len = NA)

median_no_na(x, zero_len = NA)

mean_no_na(x, zero_len = NA)

sd_no_na(x, zero_len = NA)
```

## Arguments

- x:

  The object to perform the operation on.

- FUN:

  The function to perform if the object is neither zero length or all
  `NA`

- zero_len:

  If `x` has length 0, then either return it (if `NULL`) or return `NA`
  of the same class as `x` (if `NA`)

## Value

An object of the same class as `x`

## Functions

- `max_no_na()`: Maximum

- `min_no_na()`: Minimum

- `median_no_na()`: Median

- `mean_no_na()`: Median

- `sd_no_na()`: Median

## Examples

``` r
min_no_na(c(NA, 3))
#> [1] 3
min_no_na(double(), zero_len=NULL)
#> numeric(0)
max_no_na(c(NA, 3))
#> [1] 3
```
