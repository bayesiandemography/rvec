# Create a Blank Rvec

**\[deprecated\]**

## Usage

``` r
new_rvec(x = double(), length = 0, n_draw = 1000)
```

## Arguments

- x:

  Object with the intended type. Default is
  [`double()`](https://rdrr.io/r/base/double.html).

- length:

  Desired length of rvec. Default is `0`.

- n_draw:

  Number of draws of rvec. Default is `1000`.

## Value

An rvec.

## Details

Create an rvec, consisting entirely of `NAs`, with a given length and
number of draws.

The type of the object is taken from `x`. If `typeof(x)` is `"integer"`,
for instance, then `new_rvec()` returns an object of class `"rvec_int"`.

## See also

- [`new_rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_int()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  Replacements for `rvec_new()`

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  [`rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_int()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Create an rvec from data.

- [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
  Query number of draws.

## Examples

``` r
suppressWarnings({
  new_rvec()
  new_rvec(TRUE, length = 3, n_draw = 100)

  x <- new_rvec(length = 2)
  x[1] <- rnorm_rvec(n = 1, n_draw = 1000)
  x[2] <- runif_rvec(n = 1, n_draw = 1000)
})
```
