# Extract numeric, below/above the lower/upper limits of quantification (LLQ and ULQ), and text data from a vector.

Extract numeric, below/above the lower/upper limits of quantification
(LLQ and ULQ), and text data from a vector.

## Usage

``` r
make_loq(
  x,
  llq_pattern = "^< *\\(? *([0-9]*\\.?[0-9]+) *\\)?$",
  ulq_pattern = "^> *\\(? *([0-9]*\\.?[0-9]+) *\\)?$",
  number_pattern = "^(+-)?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?$",
  replace_llq = 0,
  replace_ulq = Inf,
  ...
)
```

## Arguments

- x:

  The character vector to extract data from.

- llq_pattern, ulq_pattern:

  A regex or vector of regex values for extracting llq and ULQ. If a
  vector, all regexes will be tested.

- number_pattern:

  A regex or vector of regex values for finding numbers.

- replace_llq, replace_ulq:

  The value to place in the "number" column when a llq/ulq is matched
  (typically `0` and `Inf`, respecitvely or `NA_real_` for both).

- ...:

  Parameters passed to `grep` and `grepl` for `llq_pattern` searching.

## Value

a data.frame with columns named "text", "number", "llq", and "ulq".

## Details

If `llq_pattern` or `ulq_pattern` are regular expressions that extract a
value, then the extracted value will be used in the "llq"/"ulq" column.
If not, then the value `-1` will be inserted. If all "llq"/"ulq" values
match the `number_pattern`, then the "llq"/"ulq" columns will be
converted to numeric (if not, a warning will be given).

## Examples

``` r
make_loq(c("1", "A", "<1", ">60"))
#>   text number llq ulq
#> 1 <NA>      1  NA  NA
#> 2    A     NA  NA  NA
#> 3 <NA>      0   1  NA
#> 4 <NA>    Inf  NA  60
make_loq(c("1", "A", "<1", ">60"), replace_llq=NA_real_)
#>   text number llq ulq
#> 1 <NA>      1  NA  NA
#> 2    A     NA  NA  NA
#> 3 <NA>     NA   1  NA
#> 4 <NA>    Inf  NA  60
make_loq(c("1", "A", "<1", ">60"), replace_llq=NA_real_, replace_ulq=NA_real_)
#>   text number llq ulq
#> 1 <NA>      1  NA  NA
#> 2    A     NA  NA  NA
#> 3 <NA>     NA   1  NA
#> 4 <NA>     NA  NA  60
make_loq(c("1", "A", "<1", ">ULQ"), replace_llq=NA_real_, replace_ulq=NA_real_)
#>   text number llq ulq
#> 1 <NA>      1  NA  NA
#> 2    A     NA  NA  NA
#> 3 <NA>     NA   1  NA
#> 4 >ULQ     NA  NA  NA
make_loq(c("1", "A", "<1", ">ULQ"), replace_llq=NA_real_, replace_ulq=NA_real_, ulq_pattern=">ULQ")
#> Warning: Not all ulq values are numeric, not converting.
#>   text number llq  ulq
#> 1 <NA>      1  NA <NA>
#> 2    A     NA  NA <NA>
#> 3 <NA>     NA   1 <NA>
#> 4 <NA>     NA  NA     
```
