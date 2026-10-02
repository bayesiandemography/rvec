# Standard Deviation, Including Rvecs

Calculate standard deviation of `x`, where `x` can be an rvec. If `x` is
an rvec, separate standard deviations are calculated for each draw.

## Usage

``` r
sd(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector or R object, including an
  [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- na.rm:

  Whether to remove `NA`s before calculating standard deviations.

## Value

An rvec, if `x` is an rvec. Otherwise typically a numeric vector.

## Details

To enable different behavior for rvecs and for ordinary vectors, the
base R function [`stats::sd()`](https://rdrr.io/r/stats/sd.html) is
turned into a generic, with
[`stats::sd()`](https://rdrr.io/r/stats/sd.html) as the default.

For details on the calculations, see the documentation for
[`stats::sd()`](https://rdrr.io/r/stats/sd.html).

## See also

[`var()`](https://bayesiandemography.github.io/rvec/reference/var.md)

## Examples

``` r
x <- rvec(cbind(rnorm(10), rnorm(10, sd = 20)))
x
#> <rvec_dbl<2>[10]>
#>  [1] 0.7999,6.628   1.413,20.45    -0.989,11.02   -0.06321,28.82 0.9421,-1.484 
#>  [6] -0.5944,-2.263 0.7172,17.13   0.08513,-3.712 1.035,28.56    -1.847,41.64  
sd(x)
#> <rvec_dbl<2>[1]>
#> [1] 1.035,15.36
```
