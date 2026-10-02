# Credible Intervals and Point Estimates from Random Draws

Summarise the distribution of random draws in an rvec, using credible
intervals and point estimates.

## Usage

``` r
draws_ci(
  x,
  width = 0.95,
  prefix = NULL,
  na_rm = FALSE,
  point = c("median", "mean")
)

# S3 method for class 'rvec'
draws_ci(
  x,
  width = 0.95,
  prefix = NULL,
  na_rm = FALSE,
  point = c("median", "mean")
)

# S3 method for class 'rvec_chr'
draws_ci(
  x,
  width = 0.95,
  prefix = NULL,
  na_rm = FALSE,
  point = c("median", "mean")
)
```

## Arguments

- x:

  An
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md).

- width:

  Width(s) of credible interval(s). One or more numbers greater than 0
  and less than or equal to 1. Default is `0.95`.

- prefix:

  String to be added to the names of columns in the result. Defaults to
  name of `x`.

- na_rm:

  Whether to remove NAs before calculating summaries. Default is
  `FALSE`.

- point:

  Summary measure used for point estimate: `"median"` (the default) or
  `"mean"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
`2 * length(width) + 1` columns. When `width` has length 1, the columns
are

- `<name>.lower` Lower limit of credible interval

- `<name>.mid` Point estimate

- `<name>.upper` Upper limit of the credible interval

## Details

The credible intervals extend from the quantiles at `0.5 - 0.5 * width`
to the quantiles `0.5 + 0.5 * width`.

When `point` is `"mean"`, point estimates can lie outside the associated
credible intervals, though this only happens in unusual cases, such as
when distributions are highly skewed.

## Warning

It is tempting to assign the results of a call to `draws_ci()` to a
column in a data frame, as in

`my_df$ci <- draws_ci(my_rvec)`

However, creating columns in this way can corrupt an ordinary data
frame. For safer options, see the examples below.

## See also

[`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)
gives more options for forming quantiles.

Other ways of applying pre-specified functions across draws are:

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [draws_any](https://bayesiandemography.github.io/rvec/reference/draws_all.md)

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)

- [`draws_sd()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_var()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

- [`draws_cv()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)

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
set.seed(0)
m <- rbind(a = rnorm(100, mean = 5, sd = 2),
           b = rnorm(100, mean = -3, sd = 3),
           c = rnorm(100, mean = 0, sd = 20))
x <- rvec(m)
x
#> <rvec_dbl<100>[3]>
#>                a                b                c 
#>     4.9 (2, 8.2) -3.6 (-7.1, 2.9)    1.1 (-35, 36) 
draws_ci(x)
#> # A tibble: 3 × 3
#>   x.lower x.mid x.upper
#>     <dbl> <dbl>   <dbl>
#> 1    2.02  4.93    8.23
#> 2   -7.06 -3.61    2.87
#> 3  -35.3   1.06   36.1 
draws_ci(x, width = c(0.5, 0.99))
#> # A tibble: 3 × 5
#>   x.lower x.lower1 x.mid x.upper1 x.upper
#>     <dbl>    <dbl> <dbl>    <dbl>   <dbl>
#> 1    1.21     3.86  4.93     6.25    9.85
#> 2   -7.81    -5.60 -3.61    -1.24    4.55
#> 3  -48.1    -12.3   1.06    13.0    47.4 
draws_ci(x, point = "mean")
#> # A tibble: 3 × 3
#>   x.lower x.mid x.upper
#>     <dbl> <dbl>   <dbl>
#> 1    2.02  5.05    8.23
#> 2   -7.06 -3.14    2.87
#> 3  -35.3   1.34   36.1 
draws_ci(x, prefix = "results")
#> # A tibble: 3 × 3
#>   results.lower results.mid results.upper
#>           <dbl>       <dbl>         <dbl>
#> 1          2.02        4.93          8.23
#> 2         -7.06       -3.61          2.87
#> 3        -35.3         1.06         36.1 

## results from 'draws_ci'
## assigned to a data frame
library(dplyr)
df <- data.frame(x)

## base R approach
cbind(df, draws_ci(x))
#>                  x    x.lower     x.mid   x.upper
#> 1     4.9 (2, 8.2)   2.017346  4.934077  8.228987
#> 2 -3.6 (-7.1, 2.9)  -7.058286 -3.611295  2.872534
#> 3    1.1 (-35, 36) -35.275211  1.061243 36.052985

## a tidyverse alternative:
## mutate with no '='
df |> mutate(draws_ci(x))
#>                  x    x.lower     x.mid   x.upper
#> 1     4.9 (2, 8.2)   2.017346  4.934077  8.228987
#> 2 -3.6 (-7.1, 2.9)  -7.058286 -3.611295  2.872534
#> 3    1.1 (-35, 36) -35.275211  1.061243 36.052985
```
