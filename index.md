# rvec [![](data-raw/sticker.png)](https://github.com/bayesiandemography/rvec)

Tools for working with random draws, including draws from a simulation
or Bayesian analysis. The main data structure is an `rvec`, which holds
multiple draws but which behaves (mainly) like a standard R vector.

## Installation

``` r
install.packages("rvec")                            ## CRAN version
devtools::install_github("bayesiandemography/rvec") ## development version
```

## Example

``` r
library(rvec, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
```

Create an `rvec`.

``` r
x <- rvec(rbind(c(10, 11),
                c(100, 101),
                c(1000, 1001)))
x
#> <rvec_dbl<2>[3]>
#> [1] 10,11     100,101   1000,1001
```

Perform arithmetic on it.

``` r
x + 1
#> <rvec_dbl<2>[3]>
#> [1] 11,12     101,102   1001,1002
```

Put it in a tibble.

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

Manipulate it in a tibble.

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

Summarise it.

``` r
draws_mean(x)
#> [1]   10.5  100.5 1000.5
```

## Other packages for working with random draws

- [rv](https://CRAN.R-project.org/package=rv)
- [posterior](https://CRAN.R-project.org/package=posterior)
