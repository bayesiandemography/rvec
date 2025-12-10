# Is an Object an Rvec

Test whether `x` inherits from class `"rvec"`.

## Usage

``` r
is_rvec(x)
```

## Arguments

- x:

  An object.

## Value

`TRUE` or `FALSE`.

## See also

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  to create an rvec

- [`as.matrix()`](https://rdrr.io/r/base/matrix.html),
  [`as_list_col()`](https://bayesiandemography.github.io/rvec/reference/as_list_col.md),
  to convert an rvec into other formats

## Examples

``` r
x <- rvec_dbl()
is_rvec(x)
#> [1] TRUE
```
