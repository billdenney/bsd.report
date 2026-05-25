# Correct the case of a vector to be in a preferred case

Correct the case of a vector to be in a preferred case

## Usage

``` r
correct_case(x, preferred)
```

## Arguments

- x:

  An object to correct the case of

- preferred:

  A character vector of preferred values

## Value

\`x\` where values that match \`tolower(x) == tolower(preferred)\` are
converted to the preferred value.

## See also

Other Text standardization:
[`replace_synonym()`](https://billdenney.github.io/bsd.report/reference/replace_synonym.md)

## Examples

``` r
correct_case(c("ABC", "Abc", "aBc", "def"), "Abc")
#> [1] "Abc" "Abc" "Abc" "def"
```
