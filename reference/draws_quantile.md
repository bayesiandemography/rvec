# Quantiles Across Random Draws

Summarise the distribution of random draws in an `rvec`, using
quantiles.

## Usage

``` r
draws_quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na_rm = FALSE)

# S3 method for class 'rvec'
draws_quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na_rm = FALSE)

# S3 method for class 'rvec_chr'
draws_quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na_rm = FALSE)
```

## Arguments

- x:

  An object of class
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- probs:

  Vector of probabilities.

- na_rm:

  Whether to remove NAs before calculating summaries. Default is
  `FALSE`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Details

The `probs` argument defaults to `c(0.025, 0.25, 0.5, 0.75, 0.975)`, the
values needed for a median, a 50% credible intervals, and a 95% credible
interval.

## Warning

It is tempting to assign the results of a call to `draws_quantile()` to
a column in a data frame, as in

`my_df$quantile <- draws_quantile(my_rvec)`

However, creating data frame columns in this way can corrupt data
frames. For safer options, see the examples below.

## See also

[`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)
creates simple credible intervals.

Other functions for applying pre-specified functions across draws are:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

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
set.seed(0)
m <- rbind(a = rnorm(100, mean = 5, sd = 2),
           b = rnorm(100, mean = -3, sd = 3),
           c = rnorm(100, mean = 0, sd = 20))
x <- rvec(m)
x
#> <rvec_dbl<100>[3]>
#>                a                b                c 
#>     4.9 (2, 8.2) -3.6 (-7.1, 2.9)    1.1 (-35, 36) 
draws_quantile(x)
#> # A tibble: 3 × 5
#>    x_2.5   x_25  x_50  x_75 x_97.5
#>    <dbl>  <dbl> <dbl> <dbl>  <dbl>
#> 1   2.02   3.86  4.93  6.25   8.23
#> 2  -7.06  -5.60 -3.61 -1.24   2.87
#> 3 -35.3  -12.3   1.06 13.0   36.1 

## results from 'draws_quantile'
## assigned to a data frame
library(dplyr)
df <- data.frame(x)

## base R approach
cbind(df, draws_quantile(x))
#>                  x      x_2.5       x_25      x_50      x_75    x_97.5
#> 1     4.9 (2, 8.2)   2.017346   3.861163  4.934077  6.250702  8.228987
#> 2 -3.6 (-7.1, 2.9)  -7.058286  -5.600306 -3.611295 -1.239950  2.872534
#> 3    1.1 (-35, 36) -35.275211 -12.331310  1.061243 13.036488 36.052985

## a tidyverse alternative:
## mutate with no '='
df |>
  mutate(draws_quantile(x))
#>                  x      x_2.5       x_25      x_50      x_75    x_97.5
#> 1     4.9 (2, 8.2)   2.017346   3.861163  4.934077  6.250702  8.228987
#> 2 -3.6 (-7.1, 2.9)  -7.058286  -5.600306 -3.611295 -1.239950  2.872534
#> 3    1.1 (-35, 36) -35.275211 -12.331310  1.061243 13.036488 36.052985
```
