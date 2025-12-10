# Uniform Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the uniform distribution, modified to work with rvecs.

## Usage

``` r
dunif_rvec(x, min = 0, max = 1, log = FALSE)

punif_rvec(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)

qunif_rvec(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)

runif_rvec(n, min = 0, max = 1, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- min:

  Lower limits. Default is `0`. See
  [`stats::dunif()`](https://rdrr.io/r/stats/Uniform.html). Can be an
  rvec.

- max:

  Upper limited. Default is `1`. See
  [`stats::dunif()`](https://rdrr.io/r/stats/Uniform.html). Can be an
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

Functions `dunif_rvec()`, `punif_rvec()`, `punif_rvec()` and
`runif_rvec()` work like base R functions
[`dt()`](https://rdrr.io/r/stats/TDist.html),
[`pt()`](https://rdrr.io/r/stats/TDist.html),
[`qt()`](https://rdrr.io/r/stats/TDist.html), and
[`rt()`](https://rdrr.io/r/stats/TDist.html), except that they accept
rvecs as inputs. If any input is an rvec, then the output will be too.
Function `runif_rvec()` also returns an rvec if a value for `n_draw` is
supplied.

`dunif_rvec()`, `punif_rvec()`, `punif_rvec()` and `runif_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dunif()`](https://rdrr.io/r/stats/Uniform.html)

- [`punif()`](https://rdrr.io/r/stats/Uniform.html)

- [`qunif()`](https://rdrr.io/r/stats/Uniform.html)

- [`runif()`](https://rdrr.io/r/stats/Uniform.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(0.2, 0.5),
               c(0.6, 0.7)))
dunif_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 1,1 1,1
punif_rvec(x)
#> <rvec_dbl<2>[2]>
#> [1] 0.2,0.5 0.6,0.7

runif_rvec(n = 2,
           min = c(0, 0.5),
           n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0.49 (0.021, 0.98) 0.76 (0.52, 0.99) 
```
