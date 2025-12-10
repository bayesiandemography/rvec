# Matrix Multiplication with Rvecs

Matrix multiplication `%*%` can be used with
[rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.md).
However, in constrast to standard R vectors, multiplying an rvec by a
matrix does not produce a row or column vector. Instead it produces an
ordinary rvec, with no dimensions.

## Usage

``` r
# S3 method for class 'rvec'
matrixOps(x, y)
```

## Arguments

- x, y:

  Vectors, matrices, or rvecs.

## Value

An rvec, if `x` or `y` is an rvec.

## Examples

``` r
A <- matrix(c(10, 10, 10,
              11, 11, 11),
            nrow = 2, byrow = TRUE)
x <- rvec(list(c(1, 2),
               c(3, 4),
               c(5, 6)))
A %*% x
#> <rvec_dbl<2>[2]>
#> [1] 90,120 99,132

## matrix multiplication with an
## ordinary R matrix produces
## a row or column vector
y <- c(1, 3, 5)
A %*% y
#>      [,1]
#> [1,]   90
#> [2,]   99
```
