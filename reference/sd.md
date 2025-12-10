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
#>  [1] 1.011,10.56    0.991,-15.2    -0.5865,-24.91 0.7159,39.76   -0.7564,6.647 
#>  [6] -1.394,4.589   -2.011,6.924   -0.2756,-25.37 -0.5383,31.65  -0.2338,-10.82
sd(x)
#> <rvec_dbl<2>[1]>
#> [1] 0.9941,22.02
```
