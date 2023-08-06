#' Package 'rvec'
#'
#' Many modern statistical methods, including Bayesian methods,
#' yield draws from a distribution for the quantities of interest.
#' An enormously useful feature of draws from a distribution is
#' that they can used to generate distributions for new
#' quantities. Posterior distributions for death rates, for instance,
#' can be turned into posterior distributions for life expectancies.
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
#'
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
