# Generate LaTeX math from an R formula

Generate LaTeX math from an R formula

## Usage

``` r
# S3 method for class 'formula'
knit_print(x, inline = TRUE, replacements = list(), ..., width = 80)
```

## Arguments

- x:

  A formula

- inline:

  Should the formula be generated for use inline or as a separate line?
  (In LaTeX notation, with "\$" or "\$\$".)

- replacements:

  A named list where whenever the list name is seen as the text for any
  complete part of the equation, it is replaced with the value (which
  should be a LaTeX equation fragment).

- ...:

  Currently ignored

- width:

  Approximate number of characters for splitting to multiple lines. (Not
  yet implemented.)

## Value

A knitr `asis_output()` formula

## Examples

``` r
knit_print(a~b)
#> [1] "$a \\sim b$"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] TRUE
# The same with an equal sign
knit_print(a~b, replacements=list("\\sim"="="))
#> [1] "$a = b$"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] TRUE
knit_print(a~b/c)
#> [1] "$a \\sim \\frac{b}{c}$"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] TRUE
```
