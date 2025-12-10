# Normal Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the normal distribution, modified to work with rvecs.

## Usage

``` r
dnorm_rvec(x, mean = 0, sd = 1, log = FALSE)

pnorm_rvec(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

qnorm_rvec(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

rnorm_rvec(n, mean = 0, sd = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- mean:

  Mean of distribution. Default is `0`. See
  [`dnorm()`](https://rdrr.io/r/stats/Normal.html). Can be an rvec.

- sd:

  Standard deviation. Default is `1`. See
  [`dnorm()`](https://rdrr.io/r/stats/Normal.html). Can be an rvec.

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

Functions `dnorm_rvec()`, `pnorm_rvec()`, `pnorm_rvec()` and
`rnorm_rvec()` work like base R functions
[`dnorm()`](https://rdrr.io/r/stats/Normal.html),
[`pnorm()`](https://rdrr.io/r/stats/Normal.html),
[`qnorm()`](https://rdrr.io/r/stats/Normal.html), and
[`rnorm()`](https://rdrr.io/r/stats/Normal.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rnorm_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dnorm_rvec()`, `pnorm_rvec()`, `pnorm_rvec()` and `rnorm_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`pnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`qnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html) Base R equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(3.1, -5.4),
               c(0.1, 2.3)))
dnorm_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.003267,0.0000001857 0.397,0.02833        
pnorm_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.999,0.00000003332 0.5398,0.9893      

rnorm_rvec(n = 2,
           mean = c(-3, 3),
           sd = c(2, 4),
           n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] -2.8 (-6.7, 0.9) 3.1 (-4.4, 11)  
```
