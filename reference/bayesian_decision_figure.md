# Make a figure describing the Bayesian decision-making based on a range of lower and upper reference values

Make a figure describing the Bayesian decision-making based on a range
of lower and upper reference values

## Usage

``` r
bayesian_decision_figure(
  n,
  lrv,
  urv,
  one_sided_prob,
  sd_single_measure,
  sd_scale = c("log", "linear"),
  figure_edge = 0.05,
  figure_steps = 100,
  add_points = NULL
)
```

## Arguments

- n:

  The number of observations (per group)

- lrv, urv:

  The lower and upper reference values (target for a stop or go
  decision)

- one_sided_prob:

  The integrated probability outside of the reference value

- sd_single_measure:

  The standard deviation of a single measurement in the assessment

- sd_scale:

  Is the standard deviation on the "log" or "linear" scale

- figure_edge:

  What integrated probability should be outside of the figure edges?

- figure_steps:

  How many points should be shown in the figure?

- add_points:

  Should any specific points be added to the figure?

## Value

A list with "data" used to make the figures, "plot_prob", and
"plot_lines"
