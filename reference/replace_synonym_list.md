# Use a regular expression to choose the synonyms in a list to replace in a data.frame.

Use a regular expression to choose the synonyms in a list to replace in
a data.frame.

## Usage

``` r
replace_synonym_list(
  x,
  synonyms,
  pattern = "^synonym",
  pattern_ignore_case = TRUE,
  ...
)
```

## Arguments

- x:

  The values to possibly replace

- synonyms:

  A named character vector where the names are the verbatim value and
  the values are the values to use for replacement. When \`x\` is a
  data.frame, this may be a data.frame with columns named
  \`replacement_column\`, \`verbatim_column\`, and \`preferred_column\`
  or a list of such data.frames.

- pattern:

  The regular expression to use to choose the names from \`synonyms\` to
  use for replacement.

- pattern_ignore_case:

  Should case be ignored for name matching in \`synonyms\`?

- ...:

  Passed to \`replace_synonym()\`

## Value

The output of \`replace_synonym()\` applied to a subset of the list,
\`synonyms\`.

## See also

Other Synonyms:
[`replace_synonym()`](https://billdenney.github.io/bsd.report/reference/replace_synonym.md)
