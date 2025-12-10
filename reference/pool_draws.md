# Pool Draws

Combine draws within each combination of grouping or 'by' variables in a
data frame.

## Usage

``` r
pool_draws(data, by = NULL)

# S3 method for class 'data.frame'
pool_draws(data, by = NULL)

# S3 method for class 'grouped_df'
pool_draws(data, by = NULL)
```

## Arguments

- data:

  A data frame with one or more rvecs. Can be
  [grouped](https://dplyr.tidyverse.org/reference/group_data.html).

- by:

  The variables distingishing units after combining. Used if `data` is
  not [grouped](https://dplyr.tidyverse.org/reference/group_data.html).

## Value

A data frame.

## Details

Each combination of grouping or 'by' variables must have the same number
of rows.

## See also

- [`collapse_to_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Convert from 'draws-and-value' fromat to rvec format

- [`expand_from_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Convert from rvec format to 'draws-and-value' format

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

df <-  tibble(
  a = c(1, 1, 2, 2),
  x = rvec(list(1:2, 3:4, 5:6, 7:8))
)
df
#> # A tibble: 4 × 2
#>       a         x
#>   <dbl> <rint<2>>
#> 1     1       1,2
#> 2     1       3,4
#> 3     2       5,6
#> 4     2       7,8
df |> pool_draws(by = a)
#> # A tibble: 2 × 2
#>       a              x
#>   <dbl>      <rint<4>>
#> 1     1 2.5 (1.1, 3.9)
#> 2     2 6.5 (5.1, 7.9)
df |> group_by(a) |> pool_draws()
#> # A tibble: 2 × 2
#>       a              x
#>   <dbl>      <rint<4>>
#> 1     1 2.5 (1.1, 3.9)
#> 2     2 6.5 (5.1, 7.9)
df |> pool_draws()
#> # A tibble: 1 × 1
#>                x
#>        <rint<8>>
#> 1 4.5 (1.2, 7.8)

df_big <- tibble(
  a = c(1, 1, 2, 2, 1, 1, 2, 2),
  b = c(1, 1, 1, 1, 2, 2, 2, 2),
  x = rvec(list(1:2, 3:4, 5:6, 7:8,
                9:10, 11:12, 13:14, 15:16)),
  y = rvec(list(1:3, 4:6, 7:9, 10:12,
                13:15, 16:18, 19:21, 22:24))
)
df_big |> pool_draws(by = c(a, b))
#> # A tibble: 4 × 4
#>       a     b              x              y
#>   <dbl> <dbl>      <rint<4>>      <rint<6>>
#> 1     1     1 2.5 (1.1, 3.9) 3.5 (1.1, 5.9)
#> 2     2     1 6.5 (5.1, 7.9)  9.5 (7.1, 12)
#> 3     1     2   10 (9.1, 12)    16 (13, 18)
#> 4     2     2    14 (13, 16)    22 (19, 24)
df_big |> group_by(a, b) |> pool_draws()
#> # A tibble: 4 × 4
#>       a     b              x              y
#>   <dbl> <dbl>      <rint<4>>      <rint<6>>
#> 1     1     1 2.5 (1.1, 3.9) 3.5 (1.1, 5.9)
#> 2     2     1 6.5 (5.1, 7.9)  9.5 (7.1, 12)
#> 3     1     2   10 (9.1, 12)    16 (13, 18)
#> 4     2     2    14 (13, 16)    22 (19, 24)
df_big |> pool_draws(by = a)
#> # A tibble: 2 × 3
#>       a             x             y
#>   <dbl>     <rint<8>>    <rint<12>>
#> 1     1 6.5 (1.2, 12) 9.5 (1.3, 18)
#> 2     2  10 (5.2, 16)  16 (7.3, 24)
```
