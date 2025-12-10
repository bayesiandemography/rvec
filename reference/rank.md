# Sample Ranks, Including Rvecs

Calculate sample ranks for ordinary vectors or for rvecs. In the case of
rvecs, ranks are calculated independently for each draw.

## Usage

``` r
rank(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min")
)
```

## Arguments

- x:

  An ordinary vector or an
  [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- na.last:

  Treatment of `NA`s. Options are `TRUE`, `FALSE`, or `"keep"`. See
  [`base::rank()`](https://rdrr.io/r/base/rank.html) for details.

- ties.method:

  Treatment of ties. See
  [`base::rank()`](https://rdrr.io/r/base/rank.html) for details.

## Value

An object of class
[`rvec_int()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
if `x` is an rvec. Otherwise an ordinary integer vector.

## Details

To enable different behavior for rvecs and for ordinary vectors, the
base R function [`base::rank()`](https://rdrr.io/r/base/rank.html) is
turned into a generic, with
[`base::rank()`](https://rdrr.io/r/base/rank.html) as the default.

For details on the calculations, see the documentation for
[`base::rank()`](https://rdrr.io/r/base/rank.html).

## Examples

``` r
x <- rvec(list(c(3, 30),
               c(0, 100)))
rank(x)
#> <rvec_int<2>[2]>
#> [1] 2,1 1,2
```
