# Medians, Means, and Modes Across Random Draws

Summarise the distribution of random draws in an `rvec`, using means,
medians, or modes.

## Usage

``` r
draws_median(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_median(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_median(x, na_rm = FALSE)

draws_mean(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_mean(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_mean(x, na_rm = FALSE)

draws_mode(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_mode(x, na_rm = FALSE)
```

## Arguments

- x:

  An object of class
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- na_rm:

  Whether to remove NAs before calculating summaries. Default is
  `FALSE`.

## Value

A vector.

## Details

When `method` is `"mode"`, `reduce_rvec()` returns the most common value
for each observation. When there is a tie, it returns `NA`.

## See also

Apply pre-specified functions across draws:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)

- [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)

Apply arbitrary function across draws:

- [`draws_fun()`](https://bayesiandemography.github.io/rvec/reference/draws_fun.md)

For additional functions for summarising random draws, see
[tidybayes](https://CRAN.R-project.org/package=tidybayes) and
[ggdist](https://CRAN.R-project.org/package=ggdist). Function
[`as_list_col()`](https://bayesiandemography.github.io/rvec/reference/as_list_col.md)
converts rvecs into a format that `tidybayes` and `ggdist` can work
with.

## Examples

``` r
m <- rbind(a = c(1, 1, 1, 2, 3),
           b = c(2, 4, 0, 2, 3),
           c = c(0, 0, 1, 0, 100))
x <- rvec(m)
x
#> <rvec_dbl<5>[3]>
#>            a            b            c 
#>   1 (1, 2.9) 2 (0.2, 3.9)    0 (0, 90) 
draws_median(x)
#> a b c 
#> 1 2 0 
draws_mean(x)
#>    a    b    c 
#>  1.6  2.2 20.2 
draws_mode(x)
#> a b c 
#> 1 2 0 
```
