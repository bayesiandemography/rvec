# The Multinomial Distribution, Using Multiple Draws

Density function random generation for the multinomial distribution,
modified to work with rvecs.

## Usage

``` r
dmultinom_rvec(x, size = NULL, prob, log = FALSE)

rmultinom_rvec(n, size, prob, n_draw = NULL)
```

## Arguments

- x:

  Quantiles. Can be an rvec.

- size:

  Total number of trials. See
  [`stats::dmultinom()`](https://rdrr.io/r/stats/Multinom.html). Can be
  an rvec.

- prob:

  Numeric non-negative vector, giving the probability of each outcome.
  Internally normalized to sum to 1. See
  [`stats::dmultinom()`](https://rdrr.io/r/stats/Multinom.html). Can be
  an rvec.

- log:

  Whether to return `log(p)` rather than `p`. Default is `FALSE`. Cannot
  be an rvec.

- n:

  The length of random vector being created. Cannot be an rvec.

- n_draw:

  Number of random draws in the random vector being created. Cannot be
  an rvec.

## Value

- [`dmultinom()`](https://rdrr.io/r/stats/Multinom.html)

  - If any of the arguments are rvecs, or if a value for `n_draw` is
    supplied, then an
    [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.md)

  - Otherwise an ordinary R vector.

- [`rmultinom()`](https://rdrr.io/r/stats/Multinom.html)

  - If `n` is 1, an rvec or ordinary R vector.

  - If `n` is greater than 1, a list of rvecs or ordinary R vectors

- `rmultinom_rvec()` always returns doubles (not integers).

## Details

Functions `dmultinom_rvec()`and `rmultinom_rvec()` work like base R
functions [`dmultinom()`](https://rdrr.io/r/stats/Multinom.html) and
[`rmultinom()`](https://rdrr.io/r/stats/Multinom.html), except that they
accept rvecs as inputs. If any input is an rvec, then the output will be
too. Function `rmultinom_rvec()` also returns an rvec if a value for
`n_draw` is supplied.

Like the base R functions
[`dmultinom()`](https://rdrr.io/r/stats/Multinom.html) and
\[rmultinom(), `dmultinom_rvec()` and `rmultinom_rvec()` do not recycle
their arguments.

## Warning

From version rvec version 0.7.4 onwards, `rmultinom_rvec()` always
returns doubles (not integers).

## See also

- [`dmultinom()`](https://rdrr.io/r/stats/Multinom.html)

- [`rmultinom()`](https://rdrr.io/r/stats/Multinom.html)

- [stats::distributions](https://rdrr.io/r/stats/Distributions.html).

## Examples

``` r
x <- rvec(list(c(1, 4, 0),
               c(1, 0, 0),
               c(1, 0, 0),
               c(1, 0, 4)))
prob <- c(1/4, 1/4, 1/4, 1/4)
dmultinom_rvec(x = x, prob = prob)
#> <rvec_dbl<3>[1]>
#> [1] 0.09375,0.003906,0.003906
rmultinom_rvec(n = 1,
               size = 100,
               prob = c(0.1, 0.4, 0.2, 0.3),
               n_draw = 1000)
#> <rvec_dbl<1000>[4]>
#> [1] 10 (5, 16)  40 (31, 50) 20 (12, 28) 30 (21, 39)
```
