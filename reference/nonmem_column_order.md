# Output a data.frame with numeric columns on the left.

Output a data.frame with numeric columns on the left.

## Usage

``` r
nonmem_column_order(
  x,
  time_num_cols = c("TSFM", "TSFD", "TAD", "NTSFM", "NTSFD", "NTAD"),
  time_num_precision = 3600
)
```

## Arguments

- x:

  A data.frame or similar object

- time_num_cols:

  Columns to round to `time_num_precision`

- time_num_precision:

  Precision (`1/time_num_precision`) to use for rounding

## See also

Other Data Management:
[`check_expected_cols()`](https://billdenney.github.io/bsd.report/reference/check_expected_cols.md),
[`get_data_manage_standard_cols()`](https://billdenney.github.io/bsd.report/reference/get_data_manage_standard_cols.md)
