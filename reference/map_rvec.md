# Apply a Function and Put Results in an Rvec

Apply function `.f` to each element of `.x`, and then combine the
results into an rvec with the same length as `.x`.

## Usage

``` r
map_rvec(.x, .f, ...)
```

## Arguments

- .x:

  A vector.

- .f:

  A function.

- ...:

  Additional arguments passed to `.f`.

## Value

An [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md)
with the same length as `.x`.

## Details

Each call to function `.f` should produce an
[rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md) with
length 1.

## See also

`map_rvec()` is based on the map functions in package
[purrr](https://purrr.tidyverse.org/reference/map.html), though the
internal implementation is different.

Base R functions [`sapply()`](https://rdrr.io/r/base/lapply.html) and
[`vapply()`](https://rdrr.io/r/base/lapply.html) do not work properly
with rvecs. \[lapply() works, but to combine the results into a single
rvec, functions such as [`c()`](https://rdrr.io/r/base/c.html) or
[`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html) are
needed.

## Examples

``` r
l <- list(a = rvec(matrix(1:2, 1)),
          b = rvec(matrix(1:4, 2)),
          c = rvec(matrix(1:6, 3)))
l
#> $a
#> <rvec_int<2>[1]>
#> [1] 1,2
#> 
#> $b
#> <rvec_int<2>[2]>
#> [1] 1,3 2,4
#> 
#> $c
#> <rvec_int<2>[3]>
#> [1] 1,4 2,5 3,6
#> 
map_rvec(l, sum)
#> <rvec_int<2>[3]>
#>    a    b    c 
#>  1,2  3,7 6,15 

## sapply does not work with rvecs
sapply(l, sum)
#> $a.data
#>      [,1] [,2]
#> [1,]    1    2
#> 
#> $b.data
#>      [,1] [,2]
#> [1,]    3    7
#> 
#> $c.data
#>      [,1] [,2]
#> [1,]    6   15
#> 
```
