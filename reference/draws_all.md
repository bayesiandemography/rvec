# Logical Operations Across Random Draws

Apply `all` or `any` logical summaries across random draws.

## Usage

``` r
draws_all(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_all(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_all(x, na_rm = FALSE)

draws_any(x, na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_any(x, na_rm = FALSE)

# S3 method for class 'rvec'
draws_any(x, na_rm = FALSE)
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

## See also

Apply pre-specified functions across draws:

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

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
m <- rbind(a = c(TRUE,  FALSE,  TRUE),
           b = c(TRUE,  TRUE,   TRUE),
           c = c(FALSE, FALSE,  FALSE))
x <- rvec(m)
x
#> <rvec_lgl<3>[3]>
#>     a     b     c 
#> T,F,T T,T,T F,F,F 
draws_all(x)
#>     a     b     c 
#> FALSE  TRUE FALSE 
draws_any(x)
#>     a     b     c 
#>  TRUE  TRUE FALSE 
```
