# Create a string of the filename with its hash as an attribute to allow change detection in drake

Create a string of the filename with its hash as an attribute to allow
change detection in drake

## Usage

``` r
add_hash_to_file(filename)
```

## Arguments

- filename:

  The filename or vector of filenames to generate a hash for

## Value

\`filename\` with an attribute of \`hash\` added.

## Examples

``` r
if (FALSE) { # \dontrun{
my_plan <-
  drake_plan(
    big_filename=
      target(
        command=add_hash_to_file("a/file.xpt"),
        trigger=trigger(condition=TRUE)
      )
  )
} # }
```
