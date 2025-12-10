# Binomial Distribution, Using Multiple Draws

Density, distribution function, quantile function and random generation
for the binomial distribution, modified to work with rvecs.

## Usage

``` r
dbinom_rvec(x, size, prob, log = FALSE)

pbinom_rvec(q, size, prob, lower.tail = TRUE, log.p = FALSE)

qbinom_rvec(p, size, prob, lower.tail = TRUE, log.p = FALSE)

rbinom_rvec(n, size, prob, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- size:

  Number of trials. See
  [`dbinom()`](https://rdrr.io/r/stats/Binomial.html). Can be an rvec.

- prob:

  Probability of success in each trial. See
  [`dbinom()`](https://rdrr.io/r/stats/Binomial.html). Can be an rvec.

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
  `rbinom_rvec()` always returns doubles.

## Details

Functions `dbinom_rvec()`, `pbinom_rvec()`, `pbinom_rvec()` and
`rbinom_rvec()` work like base R functions
[`dbinom()`](https://rdrr.io/r/stats/Binomial.html),
[`pbinom()`](https://rdrr.io/r/stats/Binomial.html),
[`qbinom()`](https://rdrr.io/r/stats/Binomial.html), and
[`rbinom()`](https://rdrr.io/r/stats/Binomial.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rbinom_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

`dbinom_rvec()`, `pbinom_rvec()`, `pbinom_rvec()` and `rbinom_rvec()`
use
[tidyverse](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
vector recycling rules:

- Vectors of length 1 are recycled

- All other vectors must have the same size

## See also

- [`dbinom()`](https://rdrr.io/r/stats/Binomial.html),
  [`pbinom()`](https://rdrr.io/r/stats/Binomial.html),
  [`qbinom()`](https://rdrr.io/r/stats/Binomial.html),
  [`rbinom()`](https://rdrr.io/r/stats/Binomial.html) Base R equivalents

- [distributions](https://rdrr.io/r/stats/Distributions.html) All base R
  distributions

## Examples

``` r
x <- rvec(list(c(3, 8),
               c(0, 2)))
dbinom_rvec(x, size = 8, prob = 0.3)
#> <rvec_dbl<2>[2]>
#> [1] 0.2541,0.00006561 0.05765,0.2965   
pbinom_rvec(x, size = 8, prob = 0.3)
#> <rvec_dbl<2>[2]>
#> [1] 0.8059,1       0.05765,0.5518

rbinom_rvec(n = 2,
            size = 10,
            prob = c(0.7, 0.3),
            n_draw = 1000)
#> <rvec_dbl<1000>[2]>
#> [1] 7 (4, 9) 3 (1, 6)
```
