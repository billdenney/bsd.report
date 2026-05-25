# Print a list of ggplot objects with space around each

Print a list of ggplot objects with space around each

## Usage

``` r
# S3 method for class 'gg_list'
knit_print(x, ..., filename = NULL, fig_suffix = "\n\n")
```

## Arguments

- x:

  A list of plot objects

- ...:

  Passed to `knit_print`.

- filename:

  Save the figure to the filename, if provided. If the filename contains
  " such as " `sprintf(filename, seq_along(x))` to generate the
  filename.

- fig_suffix:

  Character strings passed to `cat` before and after printing `x` (if
  not missing).

## Value

`x` invisibly

## See also

[`knit_print.gg`](https://billdenney.github.io/bsd.report/reference/knit_print.gg.md)
