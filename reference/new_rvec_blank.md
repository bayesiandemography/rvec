# Create an Rvec Filled with a Single Value

Create an rvec that uses the same value for every element and every
draw.

## Usage

``` r
new_rvec_chr(length = 0, n_draw = 1000, value = "")

new_rvec_dbl(length = 0, n_draw = 1000, value = 0)

new_rvec_int(length = 0, n_draw = 1000, value = 0L)

new_rvec_lgl(length = 0, n_draw = 1000, value = FALSE)
```

## Arguments

- length:

  Desired length of rvec. Default is `0`.

- n_draw:

  Number of draws of rvec. Must be at least 1. Default is `1000`.

- value:

  Value used to fill the rvec. Can be `NA`. Default is `0`, `""`, or
  `FALSE`. See below for details.

## Value

An rvec.

## Details

`value` must be an atomic vector of length 1. Matrices, arrays, lists,
and rvecs are not allowed. Values are coerced to the correct type, when
the coercion can be done without losing information. Character values
are not converted to numeric or logical values.

The defaults for `value` are

- `new_rvec_chr()`: `""`

- `new_rvec_dbl()`: `0`

- `new_rvec_int()`: `0`

- `new_rvec_lgl()`: `FALSE`

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
new_rvec_dbl(length = 2, n_draw = 5, value = NA)
#> <rvec_dbl<5>[2]>
#> [1] NA (NA, NA) NA (NA, NA)
new_rvec_int(length = 3, n_draw = 5, value = 2)
#> <rvec_int<5>[3]>
#> [1] 2 (2, 2) 2 (2, 2) 2 (2, 2)

x <- new_rvec_dbl(length = 2)
x[1] <- rnorm_rvec(n = 1, n_draw = 1000)
x[2] <- runif_rvec(n = 1, n_draw = 1000)
```
