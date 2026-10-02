# Minima and Maxima Across Random Draws

Apply `min` or `max` across random draws. To summarise elements within
each draw instead, use
[`base::min()`](https://rdrr.io/r/base/Extremes.html) or
[`base::max()`](https://rdrr.io/r/base/Extremes.html).

## Usage

``` r
draws_min(x, na_rm = FALSE)

draws_max(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_min(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_min(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_max(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_max(x, na_rm = FALSE)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- na_rm:

  Whether to remove NAs before calculating minima and maxima. Default is
  `FALSE`.

## Value

A vector.

## See also

Apply pre-specified functions across draws:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_sd()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_var()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_cv()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)

- [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)

Apply arbitrary function across draws:

- [`draws_fun()`](https://bayesiandemography.github.io/rvec/reference/draws_fun.md)

Check for missing or finite values:

- [`draws_any_na()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

- [`draws_all_na()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

- [`draws_any_infinite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

- [`draws_all_infinite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

- [`draws_any_finite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

- [`draws_all_finite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)

## Examples

``` r
m <- rbind(a = c(1,  -3,  2),
           b = c(Inf,  0,   -Inf),
           c = c(0.2, 0.3,  0.1))
x <- rvec(m)
x
#> <rvec_dbl<3>[3]>
#>           a           b           c 
#>      1,-3,2  Inf,0,-Inf 0.2,0.3,0.1 
draws_min(x)
#>    a    b    c 
#> -3.0 -Inf  0.1 
draws_max(x)
#>   a   b   c 
#> 2.0 Inf 0.3 
```
