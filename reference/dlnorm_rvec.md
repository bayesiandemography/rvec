# The Log-Normal Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the log-normal distribution, modified to work with rvecs.

## Usage

``` r
dlnorm_rvec(x, meanlog = 0, sdlog = 1, log = FALSE)

plnorm_rvec(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

qlnorm_rvec(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

rlnorm_rvec(n, meanlog = 0, sdlog = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- meanlog:

  Mean of distribution, on log scale. Default is `0`. See
  [`stats::dlnorm()`](https://rdrr.io/r/stats/Lognormal.html). Can be an
  rvec.

- sdlog:

  Standard deviation of distribution, on log scale. Default is `1`. See
  [`stats::dlnorm()`](https://rdrr.io/r/stats/Lognormal.html). Can be an
  rvec.

- log, log.p:

  Whether to return results on a log scale. Default is `FALSE`. Cannot
  be an rvec.

- q:

  Quantiles. Can be an rvec.

- lower.tail:

  Whether to return \\P\[X \le x\]\\, as opposed to \\P\[X \> x\]\\.
  Default is `TRUE`. Cannot be an rvec.

- p:

  Probabilities. Can be an rvec.

- n:

  The length of random vector being created. Cannot be an rvec.

- n_draw:

  Number of random draws in the random vector being created. Cannot be
  an rvec.

## Value

- If any of the arguments are rvecs, or if a value for `n_draw` is
  supplied, then an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md)

- Otherwise an ordinary R vector.

## Details

Functions `dlnorm_rvec()`, `plnorm_rvec()`, `plnorm_rvec()` and
`rlnorm_rvec()` work like base R functions
[`dlnorm()`](https://rdrr.io/r/stats/Lognormal.html),
[`plnorm()`](https://rdrr.io/r/stats/Lognormal.html),
[`qlnorm()`](https://rdrr.io/r/stats/Lognormal.html), and
[`rlnorm()`](https://rdrr.io/r/stats/Lognormal.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rlnorm_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dlnorm_rvec()`, `plnorm_rvec()`, `plnorm_rvec()` and `rlnorm_rvec()`
use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dlnorm()`](https://rdrr.io/r/stats/Lognormal.html)

- [`plnorm()`](https://rdrr.io/r/stats/Lognormal.html)

- [`qlnorm()`](https://rdrr.io/r/stats/Lognormal.html)

- [`rlnorm()`](https://rdrr.io/r/stats/Lognormal.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3.1, 5.7),
               c(0.2, 2.3)))
dlnorm_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.06786,0.01539 0.5463,0.1226  
plnorm_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.8711,0.9591  0.05376,0.7976

rlnorm_rvec(n = 2,
            meanlog = c(1, 3),
            n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 2.8 (0.38, 18) 19 (3.1, 154) 
```
