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
#>  [1] -0.7423,31.85    0.7797,5.785     -0.7241,34.07    -2.018,25.82    
#>  [5] 0.8553,16.97     0.6489,7.008     -0.7103,-0.09679 0.7958,-1.198   
#>  [9] -1.425,1.269     1.51,25.36      
var(x)
#> <rvec_dbl<2>[1]>
#> [1] 1.362,189.2
```
