# Set a single baseline value given input data.

Set a single baseline value given input data.

## Usage

``` r
set_baseline(
  x,
  time,
  min_bl_time = -Inf,
  max_bl_time = 0,
  summaryfun = mean,
  ...
)
```

## Arguments

- x:

  A vector of observations

- time:

  A vector of times when the observations occurred

- min_bl_time, max_bl_time:

  The minum and maximum possible times for baseline measurements.

- summaryfun:

  What functions should summarize or modify the baseline value(s)?

- ...:

  Passed to \`summaryfun()\`

## Value

A scalar value of the baseline measurement

## Details

Possible baseline values are selected when:

- \`x\` is not \`NA\`

- \`time\` is not \`NA\`

- \`min_bl_time \<= time\`

- \`time \<= max_bl_time\`

If no value is possible, NA is returned (of the same class as \`x\`).
Within the possible baseline values, the values with the maximum time
are selected, and all selected values are summarized with
\`summaryfun(selected_values, ...)\`
