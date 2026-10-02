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
#>  [1] 1.595,-44.29    0.3295,22.5     -0.8205,-0.8987 0.4874,-0.3238 
#>  [5] 0.7383,18.88    0.5758,16.42    -0.3054,11.88   1.512,18.38    
#>  [9] 0.3898,15.64    -0.6212,1.491  
var(x)
#> <rvec_dbl<2>[1]>
#> [1] 0.6502,385
```
