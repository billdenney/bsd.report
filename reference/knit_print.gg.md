# Print a ggplot object with space around it

Print a ggplot object with space around it

## Usage

``` r
# S3 method for class 'gg'
knit_print(
  x,
  ...,
  fig_prefix,
  fig_suffix,
  filename = NULL,
  width = 6,
  height = 4,
  units = "in"
)
```

## Arguments

- x:

  The plot object

- ...:

  Passed to `print`.

- fig_prefix:

  See `fig_suffix`

- fig_suffix:

  Character strings passed to `cat` before and after printing `x` (if
  not missing).

- filename:

  Save the figure to the filename, if provided

- width, height, units:

  passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

`x` invisibly

## See also

[`knit_print.gg_list`](https://billdenney.github.io/bsd.report/reference/knit_print.gg_list.md)
