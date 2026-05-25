# Intermingle elements from a set of lists

Intermingle elements from a set of lists

## Usage

``` r
intermingle_list(...)
```

## Arguments

- ...:

  lists with the same length as each other.

## Value

A list with the length of the sum of the input lists.

## Examples

``` r
intermingle_list(as.list(1:5), as.list(6:10))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 6
#> 
#> [[3]]
#> [1] 2
#> 
#> [[4]]
#> [1] 7
#> 
#> [[5]]
#> [1] 3
#> 
#> [[6]]
#> [1] 8
#> 
#> [[7]]
#> [1] 4
#> 
#> [[8]]
#> [1] 9
#> 
#> [[9]]
#> [1] 5
#> 
#> [[10]]
#> [1] 10
#> 
```
