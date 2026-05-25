# Create a tibble of tested models

Create a tibble of tested models

## Usage

``` r
make_tab_models_tested(models, caption)
```

## Arguments

- models:

  A named list of fitted model objects (that can be run through the
  \`AIC()\` function)

- caption:

  The caption attribute to add to the tibble (enabling
  \`pander::pander()\` to automatically add the correct caption)

## Value

A tibble with a caption attribute. The tibble will have columns of
"Description", "AIC", and "dAIC".
