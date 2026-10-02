# Extract Draws From an Rvec

Select a set of draws, specified using indices.

## Usage

``` r
extract_draws(x, i)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- i:

  An index vector.

## Value

An rvec with the same type, length, and element names as `x`, and
`length(i)` draws. Selecting one draw still returns an rvec.

## Details

Index `i` must be a numeric vector, consisting of whole numbers between
1 and `n_draw(x)`. Duplicates are allowed. `NA`s are not. Draws are
returned in the order specified by `i`.

## See also

- [`extract_draw()`](https://bayesiandemography.github.io/rvec/reference/extract_draw.md)
  Extract one draw as an ordinary vector.

- [`thin_draws()`](https://bayesiandemography.github.io/rvec/reference/thin_draws.md)
  Randomly select draws without replacement.

- [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
  Number of draws.

## Examples

``` r
x <- rvec(matrix(1:12, nrow = 4))
x
#> <rvec_int<3>[4]>
#> [1] 1,5,9  2,6,10 3,7,11 4,8,12
extract_draws(x, c(3, 1, 3))
#> <rvec_int<3>[4]>
#> [1] 9,1,9   10,2,10 11,3,11 12,4,12

## sample with replacement
set.seed(1)
i <- sample.int(n_draw(x), size = 10, replace = TRUE)
extract_draws(x, i)
#> <rvec_int<10>[4]>
#> [1] 5 (1, 9)  6 (2, 10) 7 (3, 11) 8 (4, 12)

## shared indices preserve alignment
y <- 2 * x
extract_draws(x, i)
#> <rvec_int<10>[4]>
#> [1] 5 (1, 9)  6 (2, 10) 7 (3, 11) 8 (4, 12)
extract_draws(y, i)
#> <rvec_dbl<10>[4]>
#> [1] 10 (2, 18) 12 (4, 20) 14 (6, 22) 16 (8, 24)
```
