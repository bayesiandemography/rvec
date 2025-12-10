# The Exponential Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the exponential distribution, modified to work with rvecs.

## Usage

``` r
dexp_rvec(x, rate = 1, log = FALSE)

pexp_rvec(q, rate = 1, lower.tail = TRUE, log.p = FALSE)

qexp_rvec(p, rate = 1, lower.tail = TRUE, log.p = FALSE)

rexp_rvec(n, rate = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- rate:

  Vector of rates. See
  [`stats::dexp()`](https://rdrr.io/r/stats/Exponential.html). Can be an
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

Functions `dexp_rvec()`, `pexp_rvec()`, `pexp_rvec()` and `rexp_rvec()`
work like base R functions
[`dexp()`](https://rdrr.io/r/stats/Exponential.html),
[`pexp()`](https://rdrr.io/r/stats/Exponential.html),
[`qexp()`](https://rdrr.io/r/stats/Exponential.html), and
[`rexp()`](https://rdrr.io/r/stats/Exponential.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rexp_rvec()` also returns an rvec if a value for `n_draw`
is supplied.

`dexp_rvec()`, `pexp_rvec()`, `pexp_rvec()` and `rexp_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dexp()`](https://rdrr.io/r/stats/Exponential.html)

- [`pexp()`](https://rdrr.io/r/stats/Exponential.html)

- [`qexp()`](https://rdrr.io/r/stats/Exponential.html)

- [`rexp()`](https://rdrr.io/r/stats/Exponential.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5.1),
               c(0.1, 2.3)))
dexp_rvec(x, rate = 1.5)
#> <rvec_dbl<2>[2]>
#> [1] 0.01666,0.0007141 1.291,0.04762    
pexp_rvec(x, rate = 1.5)
#> <rvec_dbl<2>[2]>
#> [1] 0.9889,0.9995 0.1393,0.9683

rexp_rvec(n = 2,
          rate = c(1.5, 4),
          n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.46 (0.02, 2.5)    0.18 (0.0082, 0.88)
```
