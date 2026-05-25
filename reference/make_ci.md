# Make a confidence interval from a point estimate and standard error.

Make a confidence interval from a point estimate and standard error.

## Usage

``` r
make_ci(
  point,
  se,
  level = 0.95,
  transform = NULL,
  numeric = FALSE,
  format_numeric = "%0.3g",
  format_character = "%s"
)
```

## Arguments

- point:

  The point estimate (numeric vector)

- se:

  The standard error of the estimate (numeric vector)

- level:

  The confidence level

- transform:

  A function that takes a matrix input and returns a matrix output of
  the confidence interval values transformed.

- numeric:

  Should the transformed point estimate be returned instead of the
  character representation?

- format_numeric, format_character:

  Formats for use in `sprintf` for the output of the `transform`
  function.

## Value

If `numeric=FALSE` a character vector of the confidence intervals
represented as "X \[X, X\]" where X are numbers with three significant
figures (or if the transform returns a character matrix, those text). If
`numeric=TRUE`, a numeric vector of the point estimates.
