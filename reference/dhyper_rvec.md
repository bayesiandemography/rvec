# The Hypergeometric Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the hypergeometric distribution, modified to work with rvecs.

## Usage

``` r
dhyper_rvec(x, m, n, k, log = FALSE)

phyper_rvec(q, m, n, k, lower.tail = TRUE, log.p = FALSE)

qhyper_rvec(p, m, n, k, lower.tail = TRUE, log.p = FALSE)

rhyper_rvec(nn, m, n, k, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- m:

  Number of white balls in the urn. See
  [`stats::dhyper()`](https://rdrr.io/r/stats/Hypergeometric.html). Can
  be an rvec.

- n:

  Number of black balls in the urn. See
  [`stats::rhyper()`](https://rdrr.io/r/stats/Hypergeometric.html). Can
  be an rvec.

- k:

  Number of balls drawn from urn. See
  [`stats::dhyper()`](https://rdrr.io/r/stats/Hypergeometric.html). Can
  be an rvec.

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

- nn:

  The length of the random vector being created. The equivalent of `n`
  in other random variate functions. See
  [`stats::rhyper()`](https://rdrr.io/r/stats/Hypergeometric.html).
  Cannot be an rvec.

- n_draw:

  Number of random draws in the random vector being created. Cannot be
  an rvec.

## Value

- If any of the arguments are rvecs, or if a value for `n_draw` is
  supplied, then an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md)

- Otherwise an ordinary R vector.

- `rhyper_rvec()` always returns doubles (not integers).

## Details

Functions `dhyper_rvec()`, `phyper_rvec()`, `phyper_rvec()` and
`rhyper_rvec()` work like base R functions
[`dhyper()`](https://rdrr.io/r/stats/Hypergeometric.html),
[`phyper()`](https://rdrr.io/r/stats/Hypergeometric.html),
[`qhyper()`](https://rdrr.io/r/stats/Hypergeometric.html), and
[`rhyper()`](https://rdrr.io/r/stats/Hypergeometric.html), except that
they accept rvecs as inputs. If any input is an rvec, then the output
will be too. Function `rhyper_rvec()` also returns an rvec if a value
for `n_draw` is supplied.

`dhyper_rvec()`, `phyper_rvec()`, `phyper_rvec()` and `rhyper_rvec()`
use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## Warning

From version rvec version 0.7.4 onwards, `rhyper_rvec()` always returns
doubles (not integers).

## See also

- [`dhyper()`](https://rdrr.io/r/stats/Hypergeometric.html)

- [`phyper()`](https://rdrr.io/r/stats/Hypergeometric.html)

- [`qhyper()`](https://rdrr.io/r/stats/Hypergeometric.html)

- [`rhyper()`](https://rdrr.io/r/stats/Hypergeometric.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5),
               c(0, 2)))
dhyper_rvec(x, m = 6, n = 6, k = 5)
#> <rvec_dbl<2>[2]>
#> [1] 0.3788,0.007576 0.007576,0.3788
phyper_rvec(x, m = 6, n = 6, k = 5)
#> <rvec_dbl<2>[2]>
#> [1] 0.8788,1     0.007576,0.5

rhyper_rvec(nn = 2,
            k = c(3, 5),
            m = 6,
            n = 6,
            n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 1 (0, 3) 3 (1, 4)
```
