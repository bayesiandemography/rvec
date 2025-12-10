# Missing, Finite, and Infinite Values in Rvecs

Detect or remove missing and infinite values in rvecs. Operations are
done independently on each draw, though
[`na.omit()`](https://rdrr.io/r/stats/na.fail.html),
[`na.exclude()`](https://rdrr.io/r/stats/na.fail.html), and
[`na.fail()`](https://rdrr.io/r/stats/na.fail.html) also look across
draws.

## Usage

``` r
# S3 method for class 'rvec'
anyNA(x, recursive = FALSE)

# S3 method for class 'rvec'
is.na(x)

# S3 method for class 'rvec'
na.exclude(object, ...)

# S3 method for class 'rvec'
na.omit(object, ...)
```

## Arguments

- x, object:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- recursive:

  Whether [`anyNA()`](https://rdrr.io/r/base/NA.html) should be applied
  recursively to lists. Ignored when `x` is an rvec.

- ...:

  Currently ignored.

## Value

- [`anyNA()`](https://rdrr.io/r/base/NA.html) A logical rvec with length
  1.

- [`is.na()`](https://rdrr.io/r/base/NA.html),
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html),
  [`is.finite()`](https://rdrr.io/r/base/is.finite.html),
  [`is.infinite()`](https://rdrr.io/r/base/is.finite.html) A logical
  rvec with the same length as the original rvec.

- [`na.omit()`](https://rdrr.io/r/stats/na.fail.html),
  [`na.exclude()`](https://rdrr.io/r/stats/na.fail.html) An rvec with
  the same class as the original rvec, minus any elements that have
  `NA`s in any draws.

- [`na.fail()`](https://rdrr.io/r/stats/na.fail.html) The original rvec,
  or an error.

## Details

The behavior of the rvec methods for
[`is.na()`](https://rdrr.io/r/base/NA.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html),
[`is.finite()`](https://rdrr.io/r/base/is.finite.html), and
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html) differs from
the standard [vctrs](https://vctrs.r-lib.org) behavior, which is to
return a logical vector with length equal to `length(x)`. With rvecs,
the standard vctrs behavior would entail summarising across draws, which
is the job of the
[draws\_\*](https://bayesiandemography.github.io/rvec/reference/draws_all.md)
functions.

## See also

- [`if_else_rvec()`](https://bayesiandemography.github.io/rvec/reference/if_else_rvec.md)
  Modify individual values within draws

- [`is.na()`](https://rdrr.io/r/base/NA.html),
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html),
  [`is.finite()`](https://rdrr.io/r/base/is.finite.html),
  [`is.infinite()`](https://rdrr.io/r/base/is.finite.html),
  [`anyNA()`](https://rdrr.io/r/base/NA.html),
  [`na.omit()`](https://rdrr.io/r/stats/na.fail.html),
  [`na.exclude()`](https://rdrr.io/r/stats/na.fail.html) Base R
  functions

- [`vctrs::vec_detect_missing()`](https://vctrs.r-lib.org/reference/missing.html)
  Test whether all draws for an observation are missing

- [`vctrs::vec_detect_complete()`](https://vctrs.r-lib.org/reference/vec_detect_complete.html)
  Test whether any draws for an observation are missing

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md),
  [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)
  Summarise across draws

## Examples

``` r
x <- rvec(list(c(1.2, NA),
               c(Inf, 3),
               c(-1, NaN)))

## return a logical rvec
is.na(x)
#> <rvec_lgl<2>[3]>
#> [1] F,T F,F F,T
is.nan(x)
#> <rvec_lgl<2>[3]>
#> [1] F,F F,F F,T
is.finite(x)
#> <rvec_lgl<2>[3]>
#> [1] T,F F,T T,F
is.infinite(x)
#> <rvec_lgl<2>[3]>
#> [1] F,F T,F F,F

## return a logical rvec with length 1
anyNA(x)
#> <rvec_lgl<2>[1]>
#> [1] F,T

## summarise across draws
draws_any(anyNA(x))
#> [1] TRUE

## return an NA-free version of 'x'
na.omit(x)
#> <rvec_dbl<2>[1]>
#> [1] Inf,3
na.exclude(x)
#> <rvec_dbl<2>[1]>
#> [1] Inf,3

## use 'if_else_rvec' to modify values
## within rvec
if_else_rvec(is.na(x), 999, x)
#> <rvec_dbl<2>[3]>
#> [1] 1.2,999 Inf,3   -1,999 

## vctrs functions
library(vctrs, warn.conflicts = FALSE)
## all draws missing
vec_detect_missing(x)
#> [1] FALSE FALSE FALSE
## any draws missing
vec_detect_complete(x)
#> [1] FALSE  TRUE FALSE
```
