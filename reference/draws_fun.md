# Apply Summary Function Across Random Draws

Summarise the distribution of random draws in an rvec, using a function.

## Usage

``` r
draws_fun(x, fun, ...)

# S3 method for class 'rvec'
draws_fun(x, fun, ...)
```

## Arguments

- x:

  An object of class
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- fun:

  A function.

- ...:

  Additional arguments passed to `fun`.

## Value

The results from calls to `fun`, combined using
[`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html).

## See also

Apply pre-specified functions across draws:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_sd()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_var()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_cv()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)

- [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)

## Examples

``` r
set.seed(0)
m <- rbind(a = rnorm(100, mean = 5, sd = 2),
           b = rnorm(100, mean = -3, sd = 3),
           c = rnorm(100, mean = 0, sd = 20))
x <- rvec(m)
x
#> <rvec_dbl<100>[3]>
#>                a                b                c 
#>     4.9 (2, 8.2) -3.6 (-7.1, 2.9)    1.1 (-35, 36) 
draws_fun(x, fun = mad)
#>         a         b         c 
#>  1.846920  3.249964 19.154147 
draws_fun(x, fun = range)
#> $a
#> [1] 0.5521995 9.8827293
#> 
#> $b
#> [1] -8.040548  4.975974
#> 
#> $c
#> [1] -58.09798  50.14222
#> 
draws_fun(x, weighted.mean, wt = runif(100))
#>         a         b         c 
#>  5.045337 -3.136655  1.344426 
draws_fun(x, function(x) sd(x) / mean(x))
#>          a          b          c 
#>  0.3498875 -0.9234251 15.6516656 
```
