# The F Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the F distribution, modified to work with rvecs.

## Usage

``` r
df_rvec(x, df1, df2, ncp = 0, log = FALSE)

pf_rvec(q, df1, df2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

qf_rvec(p, df1, df2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

rf_rvec(n, df1, df2, ncp = 0, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- df1, df2:

  Degrees of freedom. See
  [`stats::df()`](https://rdrr.io/r/stats/Fdist.html). Can be rvecs.

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

Functions `df_rvec()`, `pf_rvec()`, `pf_rvec()` and `rf_rvec()` work
like base R functions [`df()`](https://rdrr.io/r/stats/Fdist.html),
[`pf()`](https://rdrr.io/r/stats/Fdist.html),
[`qf()`](https://rdrr.io/r/stats/Fdist.html), and
[`rf()`](https://rdrr.io/r/stats/Fdist.html), except that they accept
rvecs as inputs. If any input is an rvec, then the output will be too.
Function `rf_rvec()` also returns an rvec if a value for `n_draw` is
supplied.

`df_rvec()`, `pf_rvec()`, `pf_rvec()` and `rf_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`df()`](https://rdrr.io/r/stats/Fdist.html)

- [`pf()`](https://rdrr.io/r/stats/Fdist.html)

- [`qf()`](https://rdrr.io/r/stats/Fdist.html)

- [`rf()`](https://rdrr.io/r/stats/Fdist.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5.1),
               c(0.1, 2.3)))
df_rvec(x, df1 = 1, df2 = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.05305,0.02233 1.089,0.07765  
pf_rvec(x, df1 = 1, df2 = 3)
#> <rvec_dbl<2>[2]>
#> [1] 0.8183,0.8909 0.2274,0.7734

rf_rvec(n = 2, df1 = 1,df2 = 2:3, n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.68 (0.001, 37)  0.62 (0.0011, 17)
```
