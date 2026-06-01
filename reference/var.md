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
#>  [1] 0.6274,-11.73  -0.4372,14.32  -0.4886,-15.13 0.7173,-27.88  1.607,-40.22  
#>  [6] -0.6872,-5.511 -0.2257,-10.77 0.3673,-4.676  1.011,10.56    0.991,-15.2   
var(x)
#> <rvec_dbl<2>[1]>
#> [1] 0.5966,261
```
