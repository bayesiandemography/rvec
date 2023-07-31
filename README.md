
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvec <a href="https://github.com/bayesiandemography/rvec"><img src="data-raw/sticker.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/bayesiandemography/rvec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/rvec/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/rvec/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/rvec?branch=main)
<!-- badges: end -->

Tools for working with posterior distributions and simulation output. An
`rvec` behaves much like a standard R vector, but holds multiple random
draws.

## Installation

Install the development version of rvec from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bayesiandemography/rvec")
```

## Example

``` r
library(rvec, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

x <- rvec(rbind(c(10, 11),
                c(100, 101),
                c(1000, 1001)))
x
#> <rvec_dbl<2>[3]>
#> [1] 10,11     100,101   1000,1001
```

``` r
x + 1
#> <rvec_dbl<2>[3]>
#> [1] 11,12     101,102   1001,1002
```

``` r
df <- tibble(g = c(1, 2, 1), x)
df
#> # A tibble: 3 × 2
#>       g         x
#>   <dbl> <rdbl<2>>
#> 1     1     10,11
#> 2     2   100,101
#> 3     1 1000,1001
```

``` r
df %>%
  group_by(g) %>%
  count(wt = x)
#> # A tibble: 2 × 2
#> # Groups:   g [2]
#>       g         n
#>   <dbl> <rdbl<2>>
#> 1     1 1010,1012
#> 2     2   100,101
```
