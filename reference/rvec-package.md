# Package 'rvec'

Tools for working with random draws from a distribution, eg draws from a
posterior distribution in a Bayesian analysis.

## Details

An rvec holds multiple draws, but wherever possible behaves like an
ordinary R vector. For instance, if `x` is an rvec holding 1000 draws
from a distribution, then `2 * x` returns a new rvec where each draw has
been multiplied by 2.

To summarise across draws, use a function starting with `draws`. For
instance, to calculate a credible interval, use
[`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md).

## Functions

**Creating rvecs**

- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Class depends on input

- [`rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Doubles

- [`rvec_int()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Integers

- [`rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Logical

- [`rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  Character

- [`new_rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  Empty doubles

- [`new_rvec_int()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  Empty integers

- [`new_rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  Empty logical

- [`new_rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  Empty character

- [`collapse_to_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Data in data frame

**Manipulating rvecs**

- [`if_else_rvec()`](https://bayesiandemography.github.io/rvec/reference/if_else_rvec.md)
  [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  where `condition` is rvec

- [`map_rvec()`](https://bayesiandemography.github.io/rvec/reference/map_rvec.md)
  `map()` for rvecs

- [`extract_draw()`](https://bayesiandemography.github.io/rvec/reference/extract_draw.md)
  Single draw from rvec

- [`pool_draws()`](https://bayesiandemography.github.io/rvec/reference/pool_draws.md)
  Combine samples

**Probability distributions**

- [`dbeta_rvec()`](https://bayesiandemography.github.io/rvec/reference/dbeta_rvec.md)
  Beta

- [`dbinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dbinom_rvec.md)
  Binomial

- [`dcauchy_rvec()`](https://bayesiandemography.github.io/rvec/reference/dcauchy_rvec.md)
  Cauchy

- [`dchisq_rvec()`](https://bayesiandemography.github.io/rvec/reference/dchisq_rvec.md)
  Chi-square

- [`dexp_rvec()`](https://bayesiandemography.github.io/rvec/reference/dexp_rvec.md)
  Exponential

- [`df_rvec()`](https://bayesiandemography.github.io/rvec/reference/df_rvec.md)
  F

- [`dgamma_rvec()`](https://bayesiandemography.github.io/rvec/reference/dgamma_rvec.md)
  Gamma

- [`dgeom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dgeom_rvec.md)
  Geometric

- [`dhyper_rvec()`](https://bayesiandemography.github.io/rvec/reference/dhyper_rvec.md)
  Hypergeometric

- [`dlnorm_rvec()`](https://bayesiandemography.github.io/rvec/reference/dlnorm_rvec.md)
  Lognormal

- [`dmultinom()`](https://rdrr.io/r/stats/Multinom.html) Multinomial

- [`dnbinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dnbinom_rvec.md)
  Negative binomial

- [`dnorm_rvec()`](https://bayesiandemography.github.io/rvec/reference/dnorm_rvec.md)
  Normal

- [`dpois_rvec()`](https://bayesiandemography.github.io/rvec/reference/dpois_rvec.md)
  Poisson

- [`dt_rvec()`](https://bayesiandemography.github.io/rvec/reference/dt_rvec.md)
  Student's T

- [`dunif_rvec()`](https://bayesiandemography.github.io/rvec/reference/dunif_rvec.md)
  Uniform

- [`dweibull_rvec()`](https://bayesiandemography.github.io/rvec/reference/dweibull_rvec.md)
  Weibull

**Summarizing across draws**

- [`draws_all()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)
  All

- [`draws_any()`](https://bayesiandemography.github.io/rvec/reference/draws_all.md)
  Any

- [`draws_min()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)
  Minimum

- [`draws_max()`](https://bayesiandemography.github.io/rvec/reference/draws_min.md)
  Maximum

- [`draws_median()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
  Median

- [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
  Mean

- [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
  Modal

- [`draws_sd()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)
  Standard deviation

- [`draws_var()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)
  Variances

- [`draws_cv()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)
  Coefficients of variation

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)
  Credible intervals

- [`draws_quantile()`](https://bayesiandemography.github.io/rvec/reference/draws_quantile.md)
  Quantiles

- [`draws_fun()`](https://bayesiandemography.github.io/rvec/reference/draws_fun.md)
  Arbitrary function

- [`n_draw()`](https://bayesiandemography.github.io/rvec/reference/n_draw.md)
  Number

**Coercion, classes**

- [`as_list_col()`](https://bayesiandemography.github.io/rvec/reference/as_list_col.md)
  Rvec or matrix to list

- [`expand_from_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)
  Inverse of
  [`collapse_to_rvec()`](https://bayesiandemography.github.io/rvec/reference/collapse_to_rvec.md)

- [`is_rvec()`](https://bayesiandemography.github.io/rvec/reference/is_rvec.md)
  Object an rvec?

**Weighted summaries**

- [`weighted_mad()`](https://bayesiandemography.github.io/rvec/reference/weighted_mean.md)
  Weighted mean absolute deviation

- [`weighted_mean()`](https://bayesiandemography.github.io/rvec/reference/weighted_mean.md)
  Weighted mean

- [`weighted_median()`](https://bayesiandemography.github.io/rvec/reference/weighted_mean.md)
  Weighted median

- [`weighted_sd()`](https://bayesiandemography.github.io/rvec/reference/weighted_mean.md)
  Weighted standard deviation

- [`weighted_var()`](https://bayesiandemography.github.io/rvec/reference/weighted_mean.md)
  Weighted variances

**Datasets**

- [`divorce()`](https://bayesiandemography.github.io/rvec/reference/divorce.md)
  Divorce rates

- [`reg_post()`](https://bayesiandemography.github.io/rvec/reference/reg_post.md)
  Regression coefficients

## Packages with similar functionality

- [rv](https://CRAN.R-project.org/package=rv)

- [posterior](https://CRAN.R-project.org/package=posterior)

## See also

Useful links:

- <https://bayesiandemography.github.io/rvec/>

- <https://github.com/bayesiandemography/rvec>

- Report bugs at <https://github.com/bayesiandemography/rvec/issues>

## Author

**Maintainer**: John Bryant <john@bayesiandemography.com>

Other contributors:

- Bayesian Demography Limited \[copyright holder\]
