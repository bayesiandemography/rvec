# Matrix Multiplication with Rvecs

Matrix multiplication `%*%` can be used with
[rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.md),
provided that the version of R in use is version 4.3.0 or higher.

## Usage

``` r
# S4 method for class 'Matrix,rvec'
x %*% y

# S4 method for class 'rvec,Matrix'
x %*% y
```

## Arguments

- x, y:

  Vectors, matrices, or rvecs

## Value

An rvec if one or both of the inputs is an rvec; otherwise the default
`%*%` result.

## Details

Multiplying an rvec by a matrix produces an rvec, with no dimensions.
This is different from an ordinary R vector: multiplying an ordinary
vector by a matrix produces a row or column matrix.

## Examples

``` r
if (getRversion() >= "4.3.0") {
  A <- matrix(c(10, 10, 10,
                11, 11, 11),
              nrow = 2, byrow = TRUE)
  x <- rvec(list(c(1, 2),
                 c(3, 4),
                 c(5, 6)))
  A %*% x
}
#> <rvec_dbl<2>[2]>
#> [1] 90,120 99,132
```
