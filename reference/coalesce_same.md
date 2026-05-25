# Replace missing values with non-missing while confirming that non-missing values are the same.

Replace missing values with non-missing while confirming that
non-missing values are the same.

## Usage

``` r
coalesce_same(...)

# Default S3 method
coalesce_same(..., message_prefix = "")

# S3 method for class 'data.frame'
coalesce_same(...)
```

## Arguments

- ...:

  Values to fill in and compare

- message_prefix:

  A prefix to place before any message (e.g. error or warning) to assist
  with understanding the reason for the message.

## Value

The value filled in

## Details

For data.frames, combines columns that match names from the first
data.frame (extra columns from subsequent data.frames are ignored).

## See also

\`dplyr::coalesce\`
