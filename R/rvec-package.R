#' Package 'rvec'
#'
#' Tools for working with random draws from a distribution, eg
#' draws from a posterior distribution in a Bayesian analysis.
#'
#' An rvec holds multiple draws, but wherever possible
#' behaves like an ordinary R vector. For instance, if `x`
#' is an rvec holding 1000 draws from a distribution,
#' then `2 * x` returns a new rvec where each draw has
#' been multiplied by 2.
#'
#' To summarise across draws, use a function starting with `draws`.
#' For instance, to calculate a credible interval, use [draws_ci()].
#' 
#' @section  Functions:
#'
#' **Creating rvecs**
#'
#' - [rvec()] Class depends on input
#' - [rvec_dbl()] Doubles
#' - [rvec_int()] Integers
#' - [rvec_lgl()] Logical
#' - [rvec_chr()] Character
#' - [collapse_to_rvec()] Data in data frame
#' - [new_rvec()] Blanks
#'
#' **Manipulating rvecs**
#'
#' - [if_else_rvec()] `condition` is rvec
#' - [map_rvec()] `map()` for rvecs
#'
#' **Probability distributions**
#'
#' - [dbeta_rvec()] Beta
#' - [dbinom_rvec()] Binomial
#' - [dcauchy_rvec()] Cauchy
#' - [dchisq_rvec()] Chi-square
#' - [dexp_rvec()] Exponential
#' - [df_rvec()] F
#' - [dgamma_rvec()] Gamma
#' - [dgeom_rvec()] Geometric
#' - [dhyper_rvec()] Hypergeometric
#' - [dlnorm_rvec()] Lognormal
#' - [dmultinom()] Multinomial
#' - [dnbinom_rvec()] Negative binomial
#' - [dnorm_rvec()] Normal
#' - [dpois_rvec()] Poisson
#' - [dt_rvec()] Student's T
#' - [dunif_rvec()] Uniform
#' - [dweibull_rvec()] Weibull
#'
#' **Summarizing across draws**
#'
#' - [draws_all()] All draws
#' - [draws_any()] Any draws
#' - [draws_min()] Minimum draw
#' - [draws_max()] Maximum draw
#' - [draws_median()] Median draw
#' - [draws_mean()] Mean draw 
#' - [draws_mode()] Modal draw
#' - [draws_ci()] Credible intervals
#' - [draws_quantile()] Quantiles
#' - [draws_fun()] Arbitrary function
#' - [n_draw()] Number of draws
#'
#' **Coercion, classes**
#'
#' - [as_list_col()] Rvec or matrix to list
#' - [expand_from_rvec()] Inverse of [collapse_to_rvec()]
#' - [is_rvec()] Object an rvec?
#'
#' **Weighted summaries**
#'
#' - [weighted_mad()] Weighted mean absolute deviation
#' - [weighted_mean()] Weighted mean
#' - [weighted_median()] Weighted median
#' - [weighted_sd()] Weighted standard deviation
#' - [weighted_var()] Weighted variances
#'
#' **Datasets**
#'
#' - [divorce()] Divorce rates
#' - [reg_post()] Regression coefficients
#'
#' @section Packages with similar functionality:
#'
#' - [rv](https://CRAN.R-project.org/package=rv)
#' - [posterior](https://CRAN.R-project.org/package=posterior)
#'
#' @aliases rvec-package NULL
#' @import vctrs
#' @importFrom methods setOldClass
#' @importFrom stats median
#' @importFrom utils globalVariables
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

globalVariables('value') ## to allow use in collapse_to_rvec

## for compatibility with the S4 system
setOldClass(c("rvec_chr", "rvec", "vctrs_vctr"))
setOldClass(c("rvec_dbl", "rvec", "vctrs_vctr"))
setOldClass(c("rvec_int", "rvec", "vctrs_vctr"))
setOldClass(c("rvec_lgl", "rvec", "vctrs_vctr"))
