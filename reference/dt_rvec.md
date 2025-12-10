# Student t Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the t distribution, modified to work with rvecs.

## Usage

``` r
dt_rvec(x, df, ncp = 0, log = FALSE)

pt_rvec(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

qt_rvec(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

rt_rvec(n, df, ncp = 0, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- df:

  Degrees of freedom. See [`dt()`](https://rdrr.io/r/stats/TDist.html).
  Can be an rvec.

- ncp:

  Non-centrality parameter. Default is `0`. See
  [`dt()`](https://rdrr.io/r/stats/TDist.html). Cannot be an rvec.

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

Functions `dt_rvec()`, `pt_rvec()`, `pt_rvec()` and `rt_rvec()` work
like base R functions [`dt()`](https://rdrr.io/r/stats/TDist.html),
[`pt()`](https://rdrr.io/r/stats/TDist.html),
[`qt()`](https://rdrr.io/r/stats/TDist.html), and
[`rt()`](https://rdrr.io/r/stats/TDist.html), except that they accept
rvecs as inputs. If any input is an rvec, then the output will be too.
Function `rt_rvec()` also returns an rvec if a value for `n_draw` is
supplied.

`dt_rvec()`, `pt_rvec()`, `pt_rvec()` and `rt_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dt()`](https://rdrr.io/r/stats/TDist.html),
  [`pt()`](https://rdrr.io/r/stats/TDist.html),
  [`qt()`](https://rdrr.io/r/stats/TDist.html),
  [`rt()`](https://rdrr.io/r/stats/TDist.html) Base R equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(-3.2, 5.3),
               c(-1.6, 2)))
dt_rvec(x, df = 4)
#> <rvec_dbl<2>[2]>
#> [1] 0.01568,0.002057 0.1089,0.06629  
pt_rvec(x, df = 4)
#> <rvec_dbl<2>[2]>
#> [1] 0.01645,0.997  0.09242,0.9419

rt_rvec(n = 2,
        df = c(3, 5),
        n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.044 (-3.3, 2.7) 0.056 (-2.7, 2.6)
```
