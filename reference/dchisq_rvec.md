# The Chi-Squared Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the chi-squared distribution, modified to work with rvecs.

## Usage

``` r
dchisq_rvec(x, df, ncp = 0, log = FALSE)

pchisq_rvec(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

qchisq_rvec(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

rchisq_rvec(n, df, ncp = 0, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- df:

  Degrees of freedom. See
  [`stats::dchisq()`](https://rdrr.io/r/stats/Chisquare.html). Can be an
  rvec.

- ncp:

  Non-centrality parameter. Default is `0`. Cannot be an rvec.

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

Functions `dchisq_rvec()`, `pchisq_rvec()`, `pchisq_rvec()` and
`rchisq_rvec()` work like base R functions
[`dchisq()`](https://rdrr.io/r/stats/Chisquare.html),
[`pchisq()`](https://rdrr.io/r/stats/Chisquare.html),
[`qchisq()`](https://rdrr.io/r/stats/Chisquare.html), and
[`rchisq()`](https://rdrr.io/r/stats/Chisquare.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rchisq_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dchisq_rvec()`, `pchisq_rvec()`, `pchisq_rvec()` and `rchisq_rvec()`
use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dchisq()`](https://rdrr.io/r/stats/Chisquare.html)

- [`pchisq()`](https://rdrr.io/r/stats/Chisquare.html)

- [`qchisq()`](https://rdrr.io/r/stats/Chisquare.html)

- [`rchisq()`](https://rdrr.io/r/stats/Chisquare.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5.1),
               c(0.1, 2.3)))
dchisq_rvec(x, df = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.1542,0.07035 0.12,0.1916   
pchisq_rvec(x, df = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.6084,0.8354   0.008163,0.4875

rchisq_rvec(n = 2,
            df = 3:4,
            n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 2.4 (0.22, 9.5) 3.5 (0.55, 12) 
```
