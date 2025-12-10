# The Cauchy Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the Cauchy distribution, modified to work with rvecs.

## Usage

``` r
dcauchy_rvec(x, location = 0, scale = 1, log = FALSE)

pcauchy_rvec(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)

qcauchy_rvec(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)

rcauchy_rvec(n, location = 0, scale = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- location:

  Center of distribution. Default is `0`. See
  [`stats::dcauchy()`](https://rdrr.io/r/stats/Cauchy.html). Can be an
  rvec.

- scale:

  Scale parameter. Default is `1`. See
  [`stats::dcauchy()`](https://rdrr.io/r/stats/Cauchy.html). Can be an
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

Functions `dcauchy_rvec()`, `pcauchy_rvec()`, `pcauchy_rvec()` and
`rcauchy_rvec()` work like base R functions
[`dcauchy()`](https://rdrr.io/r/stats/Cauchy.html),
[`pcauchy()`](https://rdrr.io/r/stats/Cauchy.html),
[`qcauchy()`](https://rdrr.io/r/stats/Cauchy.html), and
[`rcauchy()`](https://rdrr.io/r/stats/Cauchy.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rcauchy_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dcauchy_rvec()`, `pcauchy_rvec()`, `pcauchy_rvec()` and
`rcauchy_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dcauchy()`](https://rdrr.io/r/stats/Cauchy.html)

- [`pcauchy()`](https://rdrr.io/r/stats/Cauchy.html)

- [`qcauchy()`](https://rdrr.io/r/stats/Cauchy.html)

- [`rcauchy()`](https://rdrr.io/r/stats/Cauchy.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, -5.1),
               c(0, -2.3)))
dcauchy_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.03183,0.01178 0.3183,0.05061 
pcauchy_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.8976,0.06163 0.5,0.1305    

rcauchy_rvec(n = 2,
             location = c(-5, 5),
             n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] -5 (-23, 9.9) 5 (-3.5, 16) 
```
