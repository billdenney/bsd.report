# Perform join where the outcome of the join is verifed to match an expected pattern.

Perform join where the outcome of the join is verifed to match an
expected pattern.

## Usage

``` r
join_control(
  x,
  y,
  join_fun,
  x_control = "any",
  y_control = "any",
  x_fraction = NA_real_,
  y_fraction = NA_real_,
  x_count = NA_integer_,
  y_count = NA_integer_,
  overlap_fraction = NA_real_,
  overlap_count = NA_integer_,
  ...
)

join_many_to_one(x, y)

join_one_to_one(x, y)
```

## Arguments

- x, y:

  tbls to join

- join_fun:

  Any function that can combine x and y (called as
  `join_fun(x, y, ...)`). Typically this will be one of
  [`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`dplyr::right_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  etc.

- x_control, y_control:

  What outcome is expected from the `x`, and `y` tbls? Default is "any"
  (see details).

- x_fraction, y_fraction, x_count, y_count:

  What fraction or count of the rows of `x` and `y` must be in the final
  data? Fractions are converted to row counts by rounding up to the
  nearest integer.

- overlap_fraction, overlap_count:

  What fraction or count of the rows of the return value must overlap
  (i.e. have rows from both) `x` and `y`? Fractions are converted to row
  counts by rounding up to the nearest integer.

- ...:

  Passed to `join_fun()`

## Value

A joined tbl

## Details

Options for `x_control` and `y_control` are below and may be combined:

- `"any"`: Any outcome is acceptable; this overrides all other options.

- `"all"`: Each row from the input must appear in the output at least
  one time.

- `"unique"`: A row may appear in the output zero or one time.

- `"missing"`: At least one row must not match in the new dataset (the
  values must be missing). This option is rarely used.

- `"nomissing"`: All rows must match in the new dataset (the values must
  not be missing).

The combination of `x_control=c("all", "unique", "nomissing")` (or
`y_control`) is a common need to confirm that all values are present
exactly one time and that there are no missing values.

## Functions

- `join_many_to_one()`: For the common task of many-to-one mapping, the
  helper function \`join_many_to_one()\` works.

- `join_one_to_one()`: For the common task of one-to-one mapping, the
  helper function \`join_one_to_one()\` works.
