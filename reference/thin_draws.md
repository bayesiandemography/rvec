# Thin Draws in an Rvec

Randomly select draws without replacement, retaining their original
order. The same draws are selected for every element of `x`.

## Usage

``` r
thin_draws(x, n_draw_new)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- n_draw_new:

  Number of draws to retain. A single whole number between 1 and
  `n_draw(x)`. Must be supplied.

## Value

An rvec with the same type, length, and element names as `x`, and
`n_draw_new` draws.

## Details

Selection uses R's random-number state; use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducibility.
When `n_draw_new` equals `n_draw(x)`, `x` is returned unchanged and no
random numbers are used.

Independently thinning related rvecs can lose draw alignment. To retain
alignment, select shared indices and use
[`extract_draws()`](https://bayesiandemography.github.io/rvec/reference/extract_draws.md)
instead.

## See also

- [`extract_draws()`](https://bayesiandemography.github.io/rvec/reference/extract_draws.md)
  Select draws using explicit indices.

- [`extract_draw()`](https://bayesiandemography.github.io/rvec/reference/extract_draw.md)
  Extract one draw as an ordinary vector.

- [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
  Number of draws.

## Examples

``` r
x <- rvec(matrix(1:40, nrow = 2))
set.seed(1)
thin_draws(x, n_draw_new = 5)
#> <rvec_int<5>[2]>
#> [1] 7 (1.2, 24) 8 (2.2, 25)
```
