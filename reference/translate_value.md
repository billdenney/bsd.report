# Convert a value from something old to something new (using gsub)

Convert a value from something old to something new (using gsub)

## Usage

``` r
translate_value(x, old, new, ...)

# S3 method for class 'data.frame'
translate_value(x, old, new, ..., exclude_col = NULL)

# S3 method for class 'factor'
translate_value(x, old, new, ...)

# S3 method for class 'character'
translate_value(x, old, new, ..., fixed = TRUE)

# Default S3 method
translate_value(x, old, new, ...)
```

## Arguments

- x:

  The object to translate

- old, new:

  The old and new character strings

- ...:

  Passed to gsub

- exclude_col:

  A vector of columns to exclude from data.frame translation

- fixed:

  Passed to gsub

## Value

The object with old converted to new

## Methods (by class)

- `translate_value(data.frame)`: Convert all columns (unless excluded)

- `translate_value(factor)`: Convert the levels

- `translate_value(character)`: Use gsub.

- `translate_value(default)`: No translation done.
