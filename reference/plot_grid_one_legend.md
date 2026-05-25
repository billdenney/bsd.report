# Extract the legend from the first figure, remove the legends from all plots, and put the legend at the end.

Extract the legend from the first figure, remove the legends from all
plots, and put the legend at the end.

## Usage

``` r
plot_grid_one_legend(...)
```

## Arguments

- ...:

  One or more ggplot2 objects

## Value

A list of the ggplot2 objects (suitable for the `plotlist` argument of
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html))

## See also

Other plot legend helpers:
[`extract_ggplot_legend()`](https://billdenney.github.io/bsd.report/reference/extract_ggplot_legend.md),
[`remove_ggplot_legend()`](https://billdenney.github.io/bsd.report/reference/remove_ggplot_legend.md)
