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
#' - [if_else_rvec()] `if_else()` where `condition` is rvec
#' - [map_rvec()] `map()` for rvecs
#' - [extract_draw()] Single draw from rvec
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
#' - [draws_all()] All
#' - [draws_any()] Any
#' - [draws_min()] Minimum
#' - [draws_max()] Maximum
#' - [draws_median()] Median
#' - [draws_mean()] Mean
#' - [draws_mode()] Modal
#' - [draws_sd()] Standard deviation
#' - [draws_var()] Variances
#' - [draws_cv()] Coefficients of variation
#' - [draws_ci()] Credible intervals
#' - [draws_quantile()] Quantiles
#' - [draws_fun()] Arbitrary function
#' - [n_draw()] Number
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
#' @importFrom methods setMethod
#' @importFrom Matrix Matrix
#' @importFrom stats median
#' @importFrom utils globalVariables
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

globalVariables('value') ## to allow use in collapse_to_rvec

