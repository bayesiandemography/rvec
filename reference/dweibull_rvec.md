# Weibull Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the Weibull distribution, modified to work with rvecs.

## Usage

``` r
dweibull_rvec(x, shape, scale = 1, log = FALSE)

pweibull_rvec(q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)

qweibull_rvec(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)

rweibull_rvec(n, shape, scale = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- shape:

  Shape parameter. See
  [`dweibull()`](https://rdrr.io/r/stats/Weibull.html). Can be an rvec.

- scale:

  Scale parameter. See
  [`dweibull()`](https://rdrr.io/r/stats/Weibull.html) Default is `1`.
  Can be an rvec.

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

Functions `dweibull_rvec()`, `pweibull_rvec()`, `pweibull_rvec()` and
`rweibull_rvec()` work like base R functions
[`dt()`](https://rdrr.io/r/stats/TDist.html),
[`pt()`](https://rdrr.io/r/stats/TDist.html),
[`qt()`](https://rdrr.io/r/stats/TDist.html), and
[`rt()`](https://rdrr.io/r/stats/TDist.html), except that they accept
rvecs as inputs. If any input is an rvec, then the output will be too.
Function `rweibull_rvec()` also returns an rvec if a value for `n_draw`
is supplied.

`dweibull_rvec()`, `pweibull_rvec()`, `pweibull_rvec()` and
`rweibull_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dweibull()`](https://rdrr.io/r/stats/Weibull.html),
  [`pweibull()`](https://rdrr.io/r/stats/Weibull.html),
  [`qweibull()`](https://rdrr.io/r/stats/Weibull.html),
  [`rweibull()`](https://rdrr.io/r/stats/Weibull.html) Base R
  equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(3.2, 4.5),
               c(0.6, 0.7)))
dweibull_rvec(x, shape = 2)
#> <rvec_dbl<2>[2]>
#> [1] 0.0002286,0.00000001445 0.8372,0.8577          
pweibull_rvec(x, shape = 2)
#> <rvec_dbl<2>[2]>
#> [1] 1,1           0.3023,0.3874

rweibull_rvec(n = 2,
              shape = c(2, 3),
              n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.82 (0.18, 2)   0.88 (0.28, 1.5)
```
