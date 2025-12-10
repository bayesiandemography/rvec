# Correlation, Variance and Covariance (Matrices), Including Rvecs

Calculate correlations and variances, including when `x` or `y` is an
rvec.

## Usage

``` r
var(x, y = NULL, na.rm = FALSE, use)
```

## Arguments

- x:

  A numeric vector, matrix, data frame, or
  [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- y:

  NULL (default) or a vector, matrix, data frame, or rvec with
  compatible dimensions to x.

- na.rm:

  Whether `NA`s removed before calculations.

- use:

  Calculation method. See
  [`stats::var()`](https://rdrr.io/r/stats/cor.html).

## Value

An rvec, if `x` or `y` is an rvec. Otherwise typically a numeric vector
or matrix.

## Details

To enable different behavior for rvecs and for ordinary vectors, the
base R function [`stats::var()`](https://rdrr.io/r/stats/cor.html) is
turned into a generic, with
[`stats::var()`](https://rdrr.io/r/stats/cor.html) as the default.

For details on the calculations, see the documentation for
[`stats::var()`](https://rdrr.io/r/stats/cor.html).

## See also

[`sd()`](https://bayesiandemography.github.io/rvec/reference/sd.md)

## Examples

``` r
x <- rvec(cbind(rnorm(10), rnorm(10, sd = 20)))
x
#> <rvec_dbl<2>[10]>
#>  [1] -0.288,5.81    -0.5489,-11.44 0.228,-6.288   1.042,-25.36   1.01,-1.054   
#>  [6] 0.536,10.69    1.258,-7.339   0.2518,11.48   -0.2787,-6.101 -0.7712,-4.832
var(x)
#> <rvec_dbl<2>[1]>
#> [1] 0.5058,121.2
```
