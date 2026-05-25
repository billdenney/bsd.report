# Expect a certain number of values in a logical vector

Expect a certain number of values in a logical vector

## Usage

``` r
expect_n(x, n = 1, msg = NULL)
```

## Arguments

- x:

  The logical vector

- n:

  The number of expected values

- msg:

  An optional message to display in case of an error

## Value

\`x\` if \`sum(x) == n\` and raises an informative error, otherwise
