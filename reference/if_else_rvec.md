# Vectorised If-Else, When Condition is an Rvec

A version of
[if_else](https://dplyr.tidyverse.org/reference/if_else.html) for the
situation where `condition` is an rvec.

## Usage

``` r
if_else_rvec(condition, true, false, missing = NULL, size = NULL)
```

## Arguments

- condition:

  An object of class
  [rvec_lgl](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- true, false:

  Vectors (including rvecs) to use for `TRUE` and `FALSE` values of
  `condition`.

- missing:

  Vectors to use for `NA` values of `condition`. Optional.

- size:

  Length of output. Optional.

## Value

An rvec with the same number of
[draws](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
as `condition`.

## See also

- base R function [`ifelse()`](https://rdrr.io/r/base/ifelse.html) does
  not not work correctly if any of the inputs are rvecs.

- **dplyr** function
  [if_else](https://dplyr.tidyverse.org/reference/if_else.html) works
  correctly if arguments `true`, `false` or `missing` are rvecs, but not
  if argument `condition` is an `rvec`.

## Examples

``` r
x <- rvec(list(c(1, 11),
               c(2, 5),
               c(22, 6)))

x > 10 ## rvec_lgl
#> <rvec_lgl<2>[3]>
#> [1] F,T F,F T,F

## if_else_rvec needed when
## 'condition' is an rvec
if_else_rvec(x > 10, 10, x)
#> <rvec_dbl<2>[3]>
#> [1] 1,10 2,5  10,6

## dplyr::if_else works when
## 'true', 'false', or 'missing'
## (but not 'condition') are rvecs
library(dplyr)
if_else(c(TRUE, FALSE, TRUE), x, 100)
#> <rvec_dbl<2>[3]>
#> [1] 1,11    100,100 22,6   
```
