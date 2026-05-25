# Print the sessionInfo in a way that is usable in knitr/rmarkdown reports

Print the sessionInfo in a way that is usable in knitr/rmarkdown reports

## Usage

``` r
# S3 method for class 'sessionInfo'
knit_print(x, ...)
```

## Arguments

- x:

  The output of sessionInfo() (if missing, it is generated)

- ...:

  Ignored

## Value

A knitr `asis_output()` version of the session info.
