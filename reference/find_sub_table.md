# When data are provided as blocks of tables within a larger data set, extract those sub-tables as a list.

When data are provided as blocks of tables within a larger data set,
extract those sub-tables as a list.

## Usage

``` r
find_sub_table(data, value_search, edge_search, ...)
```

## Arguments

- data:

  The data to extract from

- value_search:

  The data value to search for or a function to use for the search. If a
  function, it should take in the data plus the `...` arguments and
  return a two-column data frame of `row` and `column` indices to each
  of the values.

- edge_search:

  A named list with either functions to search for the edge of the table
  or an integer indicating how far to move in that direction to find the
  edge. If a function, the function should take in the data, a row index
  (integer), a column index (integer) to start the search, a direction
  (one of the values in `search_order`), and a list indicating
  `found_edges`. The function should return a single integer of the
  number of cells in the given direction. The names for the list
  elements must be "left", "right", "up", and "down", and the order of
  the list elements defines the order that they will be searched.

- ...:

  Arguments passed to user-provided search functions.

## Value

A list of data frames (or similar) with the found sub-tables.

## See also

Other Sub-table finding:
[`search_fun_edge()`](https://billdenney.github.io/bsd.report/reference/search_fun_edge.md),
[`search_fun_values_or_edge()`](https://billdenney.github.io/bsd.report/reference/search_fun_values_or_edge.md)
