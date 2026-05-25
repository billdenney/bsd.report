# Impute standard deviation from measures of dispersion.

Impute standard deviation from measures of dispersion.

## Usage

``` r
impute_sd(point, var1, var2, n, vartype)

impute_sd_sd(point, var1, var2, n, vartype)

impute_sd_cv(point, var1, var2, n, vartype)

impute_sd_se(point, var1, var2, n, vartype)

impute_sd_ci(point, var1, var2, n, vartype)

impute_sd_iqr(point, var1, var2, n, vartype)

impute_sd_range(point, var1, var2, n, vartype)
```

## Arguments

- point:

  The point estimate (measurement of central tendency)

- var1, var2:

  The first and second measures of dispersion (\`var2\` is not required
  for all imputation methods)

- n:

  The number of observations contributing to the measurement

- vartype:

  The type of dispersion (case insensitive; see Details)

## Value

A vector of imputed standard deviations

## Details

`impute_sd` selects the imputation method based on `vartype`.

`impute_sd_sd()` imputes the standard deviation from itself (not really
imputation). `var1` is the standard deviation; `var2` must be `NA`;
`vartype` must be "SD" (ignoring case); and `n` is ignored.

`impute_sd_cv()` imputes the standard deviation from the coefficient of
variation (CV). `var1` is the CV; `var2` must be `NA`; `vartype` must be
"CV", "%CV", or "CV%" (ignoring case); and `n` is ignored.

`impute_sd_se()` imputes the standard deviation from the standard error
(SE). `var1` is the standard error of the mean; `var2` must be `NA`;
`vartype` must be "SE" or "SEM" (ignoring case); and `n` is required.
Imputation assumes a sample, not population, SE.

`impute_sd_ci()` imputes the standard deviation from the confidence
interval (CI). `var1` is the lower bound of the CI; `var2` is the upper
bound of the CI; `vartype` must match the regular expression
"^(\[0-9\]+)% CI\$" (ignoring case); and `n` is required. If only an
upper or lower bound of the CI is available, set the other to `NA`.
Imputation assumes a t-distribution.

`impute_sd_iqr()` imputes the standard deviation from the inter-quartile
range (IQR). `var1` is the lower bound of the IQR; `var2` is the upper
bound of the IQR; `vartype` must be "IQR" (ignoring case); and `n` is
required. Imputation assumes a t-distribution.

`impute_sd_range()` imputes the standard deviation from the range.
`var1` is the lower bound of the range; `var2` is the upper bound of the
range; `vartype` must be "RANGE" (ignoring case); and `n` is required.
Imputation is performed by computing `(var2 - var2)/4`. This should be
the last choice of imputation methods.

## References

Cochrane Handbook, version 5.1
<https://handbook-5-1.cochrane.org/chapter_7/7_7_3_data_extraction_for_continuous_outcomes.htm>
