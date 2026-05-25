# Compute the Emax or its inverse.

Compute the Emax or its inverse.

## Usage

``` r
emax_fun(x, e0, emax, ex50, hill = 1)

inverse_emax(effect, e0, emax, ex50, hill = 1)
```

## Arguments

- x:

  The input paramter (often a concentration)

- e0:

  Baseline effect

- emax:

  Maximum effect (at the limit as `x` approaches `Inf`)

- ex50:

  Amount of x yielding 50% of the maximum effect

- hill:

  The hill slope (also known as the sigmoidicity)

- effect:

  The effect

## Value

`effect` for `emax_fun` or `x` for `inverse_emax`

## Details

The equation for the Emax function is

\$\$effect = e0 + emax \* x^{hill}/(ex50^{hill} + x^{hill})\$\$

## Functions

- `inverse_emax()`: Inverse emax
