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
#>  [1] 1.514,-35.39  0.2274,-20.74 -0.5177,31.07 -0.3688,7.23  -2.663,-1.427
#>  [6] -1.014,17.94  0.3911,23.47  -1.368,7.938  1.223,6.036   0.2707,5.42  
sd(x)
#> <rvec_dbl<2>[1]>
#> [1] 1.244,19.81
```
