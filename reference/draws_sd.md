# Standard Deviations, Variances, and Coefficients of Variation Across Random Draws

Use standard deviations, variances, or coefficients of variation to
summarise the distribution of random draws in an rvec.

## Usage

``` r
draws_sd(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_sd(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_sd(x, na_rm = FALSE)

draws_var(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_var(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_var(x, na_rm = FALSE)

draws_cv(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_cv(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_cv(x, na_rm = FALSE)
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

The coefficient of variation is the standard deviation divided by the
mean.

## See also

Apply pre-specified functions across draws:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)

- [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)

Apply arbitrary function across draws:

- [`draws_fun()`](https://bayesiandemography.github.io/rvec/reference/draws_fun.md)

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
draws_sd(x)
#>          a          b          c 
#>  0.8944272  1.4832397 44.6116577 
draws_var(x)
#>      a      b      c 
#>    0.8    2.2 1990.2 
draws_cv(x)
#>         a         b         c 
#> 0.5590170 0.6741999 2.2084979 
```
