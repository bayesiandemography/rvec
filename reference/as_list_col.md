# Convert to List Column

Convert an
[rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md) or
matrix to a list that can be used as a list column in a data frame.

## Usage

``` r
as_list_col(x)

# S3 method for class 'rvec'
as_list_col(x)

# S3 method for class 'matrix'
as_list_col(x)
```

## Arguments

- x:

  An
  [rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  or matrix.

## Value

A list:

- If `x` is an rvec, then the list contains `length(x)` vectors, each of
  which has `n_draw(x)` elements.

- If `x` is a matrix, then the list contains `nrow(x)` vectors, each of
  which has `ncol(x)` elements.

## See also

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Construct an rvec.

- [`expand_from_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Convert a data frame from 'rvec' format to 'draw-and-value' format

- Functions for summarising and plotting distributions in package
  [ggdist](https://CRAN.R-project.org/package=ggdist) use list columns
  (among other formats).

## Examples

``` r
l <- list(1:3,
          4:6)
r <- rvec(l)
as_list_col(r)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5 6
#> 
```
