# Get a vector of column names expected for a dataset.

Get a vector of column names expected for a dataset.

## Usage

``` r
get_data_manage_standard_cols(data, coltype)
```

## Arguments

- data:

  A data.frame or similar object

- coltype:

  A vector of values to match in the "Column Type" column of \`data\` or
  \`NULL\` to match all values.

## Value

A vector of the subset of "Column Name" values that are in rows of
"Column Type".

## Details

The input \`data\` must have columns named "Column Type" and "Column
Name". The "Column Type" defines arbitrary strings to be matched to
subset for the "Column Name"s of interest. The "Column Name" are the
names themselves.

If not all "Column Type" values are in \`data\[\["Column Type"\]\]\`, an
error will be raised.

## See also

Other Data Management:
[`check_expected_cols()`](https://billdenney.github.io/bsd.report/reference/check_expected_cols.md),
[`nonmem_column_order()`](https://billdenney.github.io/bsd.report/reference/nonmem_column_order.md)
