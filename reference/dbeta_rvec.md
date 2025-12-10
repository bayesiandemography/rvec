# Beta Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the Beta distribution, modified to work with rvecs.

## Usage

``` r
dbeta_rvec(x, shape1, shape2, ncp = 0, log = FALSE)

pbeta_rvec(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

qbeta_rvec(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

rbeta_rvec(n, shape1, shape2, ncp = 0, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- shape1, shape2:

  Parameters for beta distribution. Non-negative. See
  [`dbeta()`](https://rdrr.io/r/stats/Beta.html). Can be an rvec.

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
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md);
  otherwise an ordinary R vector.

## Details

Functions `dbeta_rvec()`, `pbeta_rvec()`, `pbeta_rvec()` and
`rbeta_rvec()` work like base R functions
[`dbeta()`](https://rdrr.io/r/stats/Beta.html),
[`pbeta()`](https://rdrr.io/r/stats/Beta.html),
[`qbeta()`](https://rdrr.io/r/stats/Beta.html), and
[`rbeta()`](https://rdrr.io/r/stats/Beta.html), except that they accept
rvecs as inputs. If any input is an rvec, then the output will be too.
Function `rbeta_rvec()` also returns an rvec if a value for `n_draw` is
supplied.

`dbeta_rvec()`, `pbeta_rvec()`, `pbeta_rvec()` and `rbeta_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dbeta()`](https://rdrr.io/r/stats/Beta.html),
  [`pbeta()`](https://rdrr.io/r/stats/Beta.html),
  [`qbeta()`](https://rdrr.io/r/stats/Beta.html),
  [`rbeta()`](https://rdrr.io/r/stats/Beta.html) Base R equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(0, 0.25),
               c(0.5, 0.99)))
dbeta_rvec(x, shape1 = 1, shape2 = 1)
#> <rvec_dbl<2>[2]>
#> [1] 1,1 1,1
pbeta_rvec(x, shape1 = 1, shape2 = 1)
#> <rvec_dbl<2>[2]>
#> [1] 0,0.25   0.5,0.99

rbeta_rvec(n = 2,
           shape = 1:2,
           shape2 = 1,
           n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.53 (0.025, 0.98) 0.71 (0.17, 0.99) 
```
