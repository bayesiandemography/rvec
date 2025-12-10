# Extract a Single Draw From an Rvec

Extract a single draw from `x`. If a value is supplied for `i`, extract
the `i`th draw; otherwise extract a random draw.

## Usage

``` r
extract_draw(x, i = NULL)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- i:

  Index for the draw to be extracted. A number between `1` and
  `n_draw(x)`. If no value is supplied, a draw is chosen at random.

## Value

A vector, with type

- double, if `x` has class `"rvec_dbl"`,

- integer, if `x` has class `"rvec_int"`,

- character, if `x` has class `"rvec_chr"`,

- logical, if `x` has class `"rvec_lgl"`.

## See also

[`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
Number of draws

## Examples

``` r
x <- rvec(matrix(1:50, ncol = 5))
extract_draw(x, i = 1)
#>  [1]  1  2  3  4  5  6  7  8  9 10
extract_draw(x)
#>  [1] 21 22 23 24 25 26 27 28 29 30
```
