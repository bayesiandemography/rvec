# The Geometric Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the geometric distribution, modified to work with rvecs.

## Usage

``` r
dgeom_rvec(x, prob, log = FALSE)

pgeom_rvec(q, prob, lower.tail = TRUE, log.p = FALSE)

qgeom_rvec(p, prob, lower.tail = TRUE, log.p = FALSE)

rgeom_rvec(n, prob, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- prob:

  Probability of success in each trial. See
  [`stats::dgeom()`](https://rdrr.io/r/stats/Geometric.html). Can be an
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

- `rgeom_rvec()` always returns doubles (not integers).

## Details

Functions `dgeom_rvec()`, `pgeom_rvec()`, `pgeom_rvec()` and
`rgeom_rvec()` work like base R functions
[`dgeom()`](https://rdrr.io/r/stats/Geometric.html),
[`pgeom()`](https://rdrr.io/r/stats/Geometric.html),
[`qgeom()`](https://rdrr.io/r/stats/Geometric.html), and
[`rgeom()`](https://rdrr.io/r/stats/Geometric.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rgeom_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dgeom_rvec()`, `pgeom_rvec()`, `pgeom_rvec()` and `rgeom_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## Warning

From version rvec version 0.7.4 onwards, `rgeom_rvec()` always returns
doubles (not integers).

## See also

- [`dgeom()`](https://rdrr.io/r/stats/Geometric.html)

- [`pgeom()`](https://rdrr.io/r/stats/Geometric.html)

- [`qgeom()`](https://rdrr.io/r/stats/Geometric.html)

- [`rgeom()`](https://rdrr.io/r/stats/Geometric.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(3, 5),
               c(0, 2)))
dgeom_rvec(x, prob = 0.3)
#> <rvec_dbl<2>[2]>
#> [1] 0.1029,0.05042 0.3,0.147     
pgeom_rvec(x, prob = 0.3)
#> <rvec_dbl<2>[2]>
#> [1] 0.7599,0.8824 0.3,0.657    

rgeom_rvec(n = 2,
           prob = c(0.5, 0.8),
           n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 0 (0, 6) 0 (0, 2)
```
