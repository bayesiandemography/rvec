# Negative Binomial Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the negative binomial distribution, modified to work with rvecs.

## Usage

``` r
dnbinom_rvec(x, size, prob, mu, log = FALSE)

pnbinom_rvec(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)

qnbinom_rvec(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)

rnbinom_rvec(n, size, prob, mu, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- size:

  Number of trials. See
  [`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html). Can be an
  rvec.

- prob:

  Probability of success in each trial. See
  [`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html). Can be an
  rvec.

- mu:

  Mean value. See
  [`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html). Can be an
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
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md);
  otherwise an ordinary R vector.

- Unlike base [`rbinom()`](https://rdrr.io/r/stats/Binomial.html),
  `rnbinom_rvec()` always returns doubles.

## Details

Functions `dnbinom_rvec()`, `pnbinom_rvec()`, `pnbinom_rvec()` and
`rnbinom_rvec()` work like base R functions
[`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html),
[`pnbinom()`](https://rdrr.io/r/stats/NegBinomial.html),
[`qnbinom()`](https://rdrr.io/r/stats/NegBinomial.html), and
[`rnbinom()`](https://rdrr.io/r/stats/NegBinomial.html), except that
they accept rvecs as inputs. If any input is an rvec, then the output
will be too. Function `rnbinom_rvec()` also returns an rvec if a value
for `n_draw` is supplied.

`dnbinom_rvec()`, `pnbinom_rvec()`, `pnbinom_rvec()` and
`rnbinom_rvec()` use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html),
  [`pnbinom()`](https://rdrr.io/r/stats/NegBinomial.html),
  [`qnbinom()`](https://rdrr.io/r/stats/NegBinomial.html),
  [`rnbinom()`](https://rdrr.io/r/stats/NegBinomial.html) Base R
  equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(3, 5),
               c(0, 2)))
dnbinom_rvec(x, size = 6, prob = 0.2)
#> <rvec_dbl<2>[2]>
#> [1] 0.001835,0.005285  0.000064,0.0008602
pnbinom_rvec(x, size = 6, prob = 0.2)
#> <rvec_dbl<2>[2]>
#> [1] 0.003066,0.01165  0.000064,0.001231

rnbinom_rvec(n = 2,
             size = 2,
             mu = c(4, 8),
             n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 3 (0, 13) 7 (0, 25)
```
