# Sequence generation on an expoential scale

Sequence generation on an expoential scale

## Usage

``` r
seq_exp(
  from = 1,
  to = 1,
  length.out = NULL,
  by = NULL,
  add_values = double(length = 0),
  ...
)
```

## Arguments

- from, to:

  the starting and (maximal) end values of the sequence. Of length `1`
  unless just `from` is supplied as an unnamed argument.

- length.out:

  desired length of the sequence. A non-negative number, which for `seq`
  and `seq.int` will be rounded up if fractional.

- by:

  Explicitly ignored

- add_values:

  Add more values to the sequence

- ...:

  Passed to \`base::seq()\`

## Value

A sorted sequence

## Details

If \`from\` is zero, then it will be modified to be
\`to/(100\*length.out)\` and zero will be added to the \`add_values\`
vector.
