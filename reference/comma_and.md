# Generate a list of text grammatically correctly

Generate a list of text grammatically correctly

## Usage

``` r
comma_and(x, oxford_comma = TRUE, conjunction = "and")
```

## Arguments

- x:

  a vector to be put into a list.

- oxford_comma:

  Should the Oxford comma be used?

- conjunction:

  The conjunction to use (without spaces). `conjunction` will usually be
  "and" or "or".

## Value

A character string of the vector input separated by commas and the word
"and" as appropriate.
