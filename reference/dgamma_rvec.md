# The Gamma Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the gamma distribution, modified to work with rvecs.

## Usage

``` r
dgamma_rvec(x, shape, rate = 1, scale = 1/rate, log = FALSE)

pgamma_rvec(
  q,
  shape,
  rate = 1,
  scale = 1/rate,
  lower.tail = TRUE,
  log.p = FALSE
)

qgamma_rvec(
  p,
  shape,
  rate = 1,
  scale = 1/rate,
  lower.tail = TRUE,
  log.p = FALSE
)

rgamma_rvec(n, shape, rate = 1, scale = 1/rate, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- shape:

  Shape parameter. See
  [`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html). Can be an
  rvec.

- rate:

  Rate parameter. See
  [`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html). Can be an
  rvec.

- scale:

  Scale parameter. An alterative to `rate`. See
  [`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html). Can be an
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

Functions `dgamma_rvec()`, `pgamma_rvec()`, `pgamma_rvec()` and
`rgamma_rvec()` work like base R functions
[`dgamma()`](https://rdrr.io/r/stats/GammaDist.html),
[`pgamma()`](https://rdrr.io/r/stats/GammaDist.html),
[`qgamma()`](https://rdrr.io/r/stats/GammaDist.html), and
[`rgamma()`](https://rdrr.io/r/stats/GammaDist.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rgamma_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dgamma_rvec()`, `pgamma_rvec()`, `pgamma_rvec()` and `rgamma_rvec()`
use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dgamma()`](https://rdrr.io/r/stats/GammaDist.html)

- [`pgamma()`](https://rdrr.io/r/stats/GammaDist.html)

- [`qgamma()`](https://rdrr.io/r/stats/GammaDist.html)

- [`rgamma()`](https://rdrr.io/r/stats/GammaDist.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5.1),
               c(0.1, 2.3)))
dgamma_rvec(x, shape = 1)
#> <rvec_dbl<2>[2]>
#> [1] 0.04979,0.006097 0.9048,0.1003   
pgamma_rvec(x, shape = 1)
#> <rvec_dbl<2>[2]>
#> [1] 0.9502,0.9939  0.09516,0.8997

rgamma_rvec(n = 2,
            shape = 1,
            rate = c(0.5, 1),
            n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 1.4 (0.045, 7.3)  0.67 (0.026, 3.9)
```
