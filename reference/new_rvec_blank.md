# Create an Empty Rvec

Create an rvec, filled with `0`, `""`, or `FALSE`, with a given length
or number of draws.

## Usage

``` r
new_rvec_chr(length = 0, n_draw = 1000)

new_rvec_dbl(length = 0, n_draw = 1000)

new_rvec_int(length = 0, n_draw = 1000)

new_rvec_lgl(length = 0, n_draw = 1000)
```

## Arguments

- length:

  Desired length of rvec. Default is `0`.

- n_draw:

  Number of draws of rvec. Default is `1000`.

## Value

An rvec.

## See also

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_int()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Create an rvec from data.

- [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
  Query number of draws.

## Examples

``` r
new_rvec_int()
#> <rvec_int<1000>[0]>
new_rvec_lgl(length = 1, n_draw = 5)
#> <rvec_lgl<5>[1]>
#> [1] p=0

x <- new_rvec_dbl(length = 2)
x[1] <- rnorm_rvec(n = 1, n_draw = 1000)
x[2] <- runif_rvec(n = 1, n_draw = 1000)
```
