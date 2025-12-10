# The Poisson Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the Poisson distribution, modified to work with rvecs.

## Usage

``` r
dpois_rvec(x, lambda, log = FALSE)

ppois_rvec(q, lambda, lower.tail = TRUE, log.p = FALSE)

qpois_rvec(p, lambda, lower.tail = TRUE, log.p = FALSE)

rpois_rvec(n, lambda, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- lambda:

  Vector of means. See
  [`stats::rpois()`](https://rdrr.io/r/stats/Poisson.html). Can be an
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

- `rpois_rvec()` always returns doubles (not integers).

## Details

Functions `dpois_rvec()`, `ppois_rvec()`, `ppois_rvec()` and
`rpois_rvec()` work like base R functions
[`dpois()`](https://rdrr.io/r/stats/Poisson.html),
[`ppois()`](https://rdrr.io/r/stats/Poisson.html),
[`qpois()`](https://rdrr.io/r/stats/Poisson.html), and
[`rpois()`](https://rdrr.io/r/stats/Poisson.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rpois_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dpois_rvec()`, `ppois_rvec()`, `ppois_rvec()` and `rpois_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## Warning

From version rvec version 0.7.4 onwards, `rpois_rvec()` always returns
doubles (not integers).

## See also

- [`dpois()`](https://rdrr.io/r/stats/Poisson.html)

- [`ppois()`](https://rdrr.io/r/stats/Poisson.html)

- [`qpois()`](https://rdrr.io/r/stats/Poisson.html)

- [`rpois()`](https://rdrr.io/r/stats/Poisson.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5),
               c(1, 2)))
dpois_rvec(x, lambda = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.224,0.1008 0.1494,0.224
ppois_rvec(x, lambda = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.6472,0.9161 0.1991,0.4232

rpois_rvec(n = 2,
           lambda = c(5, 10),
           n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 5 (1, 10)  10 (4, 16)
```
