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
#' - [rvec()] - class depends on input
#' - [rvec_dbl()] - doubles
#' - [rvec_int()] - integers
#' - [rvec_lgl()] - logical
#' - [rvec_chr()] - character
#' - [collapse_to_rvec()] - data in data frame
#'
#' **Manipulating rvecs**
#'
#' - [if_else_rvec()]
#' - [map_rvec()]
#'
#' **Probability distributions**
#'
#' - [dbeta_rvec()]
#' - [dbinom_rvec()]
#' - [dcauchy_rvec()]
#' - [dchisq_rvec()]
#' - [dexp_rvec()]
#' - [df_rvec()]
#' - [dgamma_rvec()]
#' - [dgeom_rvec()]
#' - [dhyper_rvec()]
#' - [dlnorm_rvec()]
#' - [dmultinom()]
#' - [dnbinom_rvec()]
#' - [dnorm_rvec()]
#' - [dpois_rvec()]
#' - [dt_rvec()]
#' - [dunif_rvec()]
#' - [dweibull_rvec()]
#'
#' **Summarizing across draws**
#'
#' - [draws_all()]
#' - [draws_any()]
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_ci()]
#' - [draws_quantile()]
#' - [draws_fun()]
#' - [n_draw()]
#'
#' **Coercion, classes**
#'
#' - [as_list_col()]
#' - [expand_from_rvec()]
#' - [is_rvec()]
#'
#' **Weighted summaries**
#'
#' - [weighted_mad()]
#' - [weighted_mean()]
#' - [weighted_median()]
#' - [weighted_sd()]
#' - [weighted_var()]
#'
#' **Datasets**
#'
#' - [divorce()]
#' - [reg_post()]
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
