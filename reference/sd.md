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
#>  [1] -0.6756,3.541  -0.2539,-40.32 -2.547,1.06    -0.3136,40.41  -0.9323,-18.41
#>  [6] -0.5848,11.37  0.1512,-36.84  -1.284,1.142   -0.5094,30.32  -2.338,6.876  
sd(x)
#> <rvec_dbl<2>[1]>
#> [1] 0.8875,25.96
```
