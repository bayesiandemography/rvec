# Missing and Finite Values Across Draws

Test whether draws are missing, infinite, or finite. Tests can apply to
all draws or any draws.

## Usage

``` r
draws_any_na(x)

# S3 method for class 'rvec'
draws_any_na(x)

draws_all_na(x)

# S3 method for class 'rvec'
draws_all_na(x)

draws_any_infinite(x)

# S3 method for class 'rvec'
draws_any_infinite(x)

# S3 method for class 'rvec_chr'
draws_any_infinite(x)

draws_all_infinite(x)

# S3 method for class 'rvec'
draws_all_infinite(x)

# S3 method for class 'rvec_chr'
draws_all_infinite(x)

draws_any_finite(x)

# S3 method for class 'rvec'
draws_any_finite(x)

# S3 method for class 'rvec_chr'
draws_any_finite(x)

draws_all_finite(x)

# S3 method for class 'rvec'
draws_all_finite(x)

# S3 method for class 'rvec_chr'
draws_all_finite(x)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

## Value

A logical vector of length `length(x)`.

## Details

Missing values include `NA` and `NaN`; infinite values are `Inf` and
`-Inf`; finite values exclude all four. Missingness checks accept all
rvec types, but finiteness checks reject character rvecs. Results never
contain `NA`.

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

Test for missing or finite values separately across each element and
draw:

- [`is.na()`](https://rdrr.io/r/base/NA.html)

- [`is.finite()`](https://rdrr.io/r/base/is.finite.html)

- [`is.infinite()`](https://rdrr.io/r/base/is.finite.html)

## Examples

``` r
x <- rvec(rbind(complete1 = c(1, 2, 3),
                missing = c(1, NA, NaN),
                unbounded = c(Inf, -Inf, 2),
                complete2 = c(11, 12, 13)))
x
#> <rvec_dbl<3>[4]>
#>   complete1     missing   unbounded   complete2 
#>       1,2,3 1,  NA, NaN  Inf,-Inf,2    11,12,13 

draws_any_na(x)
#> complete1   missing unbounded complete2 
#>     FALSE      TRUE     FALSE     FALSE 
draws_all_na(x)
#> complete1   missing unbounded complete2 
#>     FALSE     FALSE     FALSE     FALSE 
draws_any_infinite(x)
#> complete1   missing unbounded complete2 
#>     FALSE     FALSE      TRUE     FALSE 
draws_all_infinite(x)
#> complete1   missing unbounded complete2 
#>     FALSE     FALSE     FALSE     FALSE 
draws_any_finite(x)
#> complete1   missing unbounded complete2 
#>      TRUE      TRUE      TRUE      TRUE 
draws_all_finite(x)
#> complete1   missing unbounded complete2 
#>      TRUE     FALSE     FALSE      TRUE 

# Keep elements whose draws are all finite
x[draws_all_finite(x)]
#> <rvec_dbl<3>[2]>
#> complete1 complete2 
#>     1,2,3  11,12,13 

# Filter rows of a data frame using the same condition
df <- tibble::tibble(id = seq_along(x), value = x)
df[draws_all_finite(df$value), ]
#> # A tibble: 2 × 2
#>      id     value
#>   <int> <rdbl<3>>
#> 1     1     1,2,3
#> 2     4  11,12,13

# draws_any_na() vs anyNA()
draws_any_na(x) # aggregates within elements, across draws
#> complete1   missing unbounded complete2 
#>     FALSE      TRUE     FALSE     FALSE 
anyNA(x)        # aggregates across elements, within draws
#> <rvec_lgl<3>[1]>
#> [1] F,T,T
```
