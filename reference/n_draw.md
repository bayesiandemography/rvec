# Query Number of Draws

Get a count of the random draws held by `x`. If `x` does not hold random
draws, then `n_draw()` throws an error.

## Usage

``` r
n_draw(x)

# Default S3 method
n_draw(x)

# S3 method for class 'rvec'
n_draw(x)
```

## Arguments

- x:

  An object that holds random draws, eg an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

## Value

An integer scalar.

## See also

- [`is_rvec()`](https://bayesiandemography.github.io/rvec/reference/is_rvec.md)
  to test if an object is an rvec.

## Examples

``` r
m <- matrix(1:40, nrow = 4, ncol = 10)
x <- rvec(m)
n_draw(x)
#> [1] 10
```
