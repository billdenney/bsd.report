# Verify that a data.frame has the expected columns present

Verify that a data.frame has the expected columns present

## Usage

``` r
check_expected_cols(data, cols)
```

## Arguments

- data:

  A data.frame or similar object

- cols:

  A character vector of expected column names

## Value

\`data\` where the columns are ordered according to the order in
\`cols\`.

## Details

Either more or fewer columns are an error.

## See also

Other Data Management:
[`get_data_manage_standard_cols()`](https://billdenney.github.io/bsd.report/reference/get_data_manage_standard_cols.md),
[`nonmem_column_order()`](https://billdenney.github.io/bsd.report/reference/nonmem_column_order.md)
