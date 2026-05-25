# Go to a value or the edge

Go to a value or the edge

## Usage

``` r
search_fun_values_or_edge(
  values,
  from = c("row", "column"),
  skip = c(0, 0),
  exclude_value = FALSE
)
```

## Arguments

- values:

  The value to search for

- from:

  The location to search from within the block. It must already be
  found. Options pairs of "up", "row", or "down" and "left", "column",
  or "right". Defaults to the column and row of the found value.

- skip:

  The rows and columns to skip from the `from` argument when starting
  the search.

- exclude_value:

  Exclude the found value from the edge? (Useful if you are searching
  for an NA cell at the edge and do not want that NA value in the
  result.)

## Value

A number for the distance to the edge.

## See also

Other Sub-table finding:
[`find_sub_table()`](https://billdenney.github.io/bsd.report/reference/find_sub_table.md),
[`search_fun_edge()`](https://billdenney.github.io/bsd.report/reference/search_fun_edge.md)
