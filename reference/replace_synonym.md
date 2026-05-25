# Replace values in a character string with their synonym

Replace values in a character string with their synonym

## Usage

``` r
replace_synonym(x, synonyms, ignore_case = TRUE, ...)

# S3 method for class 'character'
replace_synonym(x, synonyms, ignore_case = TRUE, ...)

# S3 method for class 'data.frame'
replace_synonym(
  x,
  synonyms,
  ignore_case = TRUE,
  ...,
  replacement_column = "Column",
  verbatim_column = "Verbatim",
  preferred_column = "Preferred"
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

- ignore_case:

  Should the synonyms be replaced case-insensitively?

- ...:

  Passed to other \`replace_synonym()\` methods.

- replacement_column:

  The column name in \`synonyms\` to find where in \`x\` to apply
  synonyms.

- verbatim_column:

  The column name in \`synonyms\` with verbatim terms.

- preferred_column:

  The column name in \`synonyms\` with preferred terms.

## Value

\`x\` with preferred values instead of verbatim values.

## See also

Other Text standardization:
[`correct_case()`](https://billdenney.github.io/bsd.report/reference/correct_case.md)

Other Synonyms:
[`replace_synonym_list()`](https://billdenney.github.io/bsd.report/reference/replace_synonym_list.md)

## Examples

``` r
replace_synonym(
  c("A", "B", "C", "a"),
   c(A="apple", B="bear", C="cabbage")
)
#> [1] "apple"   "bear"    "cabbage" "apple"  
replace_synonym(
  c("A", "B", "C", "a"),
  c(A="apple", B="bear", C="cabbage"),
  ignore_case=FALSE
)
#> [1] "apple"   "bear"    "cabbage" "a"      
replace_synonym(
  x=
    data.frame(
      A=rep(c("A", "B"), each=2),
      B=letters[1:4],
      stringsAsFactors=FALSE
    ),
    synonyms=
      data.frame(
        A="A",
        Column="B",
        Verbatim=c("a", "c"),
        Preferred=c("apple", "cherry"),
        stringsAsFactors=FALSE
      )
)
#> # A tibble: 4 × 2
#>   A     B    
#>   <chr> <chr>
#> 1 A     apple
#> 2 A     b    
#> 3 B     c    
#> 4 B     d    
```
