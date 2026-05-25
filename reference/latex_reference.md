# Generate a latex label or reference to the label

Generate a latex label or reference to the label

## Usage

``` r
latex_reference(x)

latex_label(x)

latex_label_clean(x)

latex_label_first_last(
  object,
  text,
  sep = "-",
  suffix_text = c("first", "last")
)
```

## Arguments

- x:

  The label (a character vector, usually starting with "fig:" for
  figures and "tab:" for tables)

- object:

  The object (with a length \> 1)

- text:

  The text to use for the start of the label (usually "fig:xxx" or
  "tab:xxx")

- sep:

  The separator to use between the text and the suffix

- suffix_text:

  a 2-long character string with the text to use after the separator

## Value

A knitr asis_output label or reference

A character vector with latex labels for the first and last objects and
blank strings in between.

## Functions

- `latex_label()`: Generate a latex label (location to be found by a
  reference).

- `latex_label_clean()`: Clean latex labels so that they are usable
  (remove spaces and pass through
  [`Hmisc::latexTranslate`](https://rdrr.io/pkg/Hmisc/man/latex.html)).

- `latex_label_first_last()`: Generate a latex label for the first and
  last of a list of objects with blanks between
