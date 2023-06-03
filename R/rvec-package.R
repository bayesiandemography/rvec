#' Package 'rvec'
#'
#' Many modern statistical methods, including Bayesian methods,
#' yield draws from a distribution for the quantities of interest.
#' An enormously useful feature of draws from a distribution is
#' that they can used to generate distributions for new
#' quantities. Posterior distributions for death rates, for instance,
#' can be turned into posterior distributions for life expectancies.
#'
#'  #' An `rvec`
#'
#' |              | Draw 1        | Draw 2        | \eqn{\dots}  | Draw \eqn{n}  |
#' |:-------------|:-------------:|:-------------:|:------------:|:-------------:|
#' | Obs 1        | \eqn{x_{1,1}} | \eqn{x_{1,2}} | \eqn{\dots}  | \eqn{x_{1,n}} |
#' | Obs 2        | \eqn{x_{2,1}} | \eqn{x_{2,2}} | \eqn{\dots}  | \eqn{x_{2,n}} |
#' | \eqn{\vdots} | \eqn{\vdots}  | \eqn{\vdots}  | \eqn{\ddots} | \eqn{\vdots}  |
#' | Obs \eqn{m}  | \eqn{x_{m,1}} | \eqn{x_{m,2}} | \eqn{\dots}  | \eqn{x_{m,n}} |
#'

#'
#'
#'
#' Starting with a sample
#'
#' \deqn{x_1, x_2, \dots, x_n}
#'
#' we apply one or more functions to each draw,
#' to obtain a sample
#'
#' \deqn{f(g(x_1)), f(g(x_2)), \dots, f(g(x_n))},
#'
#' from the distribution of the  quantity \eqn{f(g(x))}.
#' This derived distribution can, like any distribution,
#' be summarised through indicators such as means and
#' quantiles.
#'
#' Package `rvec` makes  recipe easy to follow.
#'
#' @section Standard workflow:
#'
#' **Step 1: Create an `rvec`**
#'
#' Create an `rvec` object holding draws from a posterior sample.
#' 
#' - [rvec()] - class depends on input
#' - [rvec_dbl()] - doubles
#' - [rvec_int()] - integers
#' - [rvec_lgl()] - logical
#' - [rvec_chr()] - character
#'
#'
#' **Step 2: Manipulate the `rvec`**
#'
#' Perform calculations on the `rvec`, treating it like an ordinary vector.
#' Internally, the calculations the manipulations are applied independenly
#' to each draw.
#'
#' - Standard mathematical and statistical operations
#' - Special rvec operations
#'
#'
#' **Step 3: Summarise the `rvec`**
#'
#' - [draws_median()]
#' - [draws_mean()]
#' - [draws_mode()]
#' - [draws_quantile()]
#' - [draws_fun()]
#' 
#'
#' @section Other functions:
#'
#' - [collapse_to_rvec()]
#' - [collapse_to_rvec()]
#'
#' @section Design:
#'
#' `rvec` are built from [vctrs][vctrs::vctrs] package, which
#' look after many of the tricky details, and give it a sound
#' conceptual basis. Internally, many of the calculations are
#' done using the [matrixStats][matrixStats::matrixStats-package]
#' package, is fast.
#'
#'
#' @section Other packages:
#'
#' posterior: different type of data structure,
#' but also lots of diagnostics
#'
#' ggdist
#'
#' distributional
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
