# Patch one data set using another

Patch one data set using another

## Usage

``` r
patch_data(
  basedata,
  patchdata,
  by = dplyr::group_vars(basedata),
  ...,
  replace = NA,
  do_not_replace = NA,
  verbose = TRUE
)
```

## Arguments

- basedata:

  The original (source) data

- patchdata:

  The updated data

- by:

  The variables to match between source and updated data (all other
  columns in the updated data are used to patch the source data).

- ...:

  Ignored

- replace:

  Values selected in `basedata` for replacement with values in
  `patchdata` (via `%in%`). (If `NULL`, all values may be replaced.)

- do_not_replace:

  Values in `patchdata` not to use for replacement of values in
  `basedata`. (If `NULL`, all values may be used for replacement.)

- verbose:

  Report on replacement count by column.

## Value

The `basedata` updated with values from `patchdata`
