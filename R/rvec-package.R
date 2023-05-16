#' Package 'rvec'
#'
#' Many modern statistical methods, including Bayesian methods,
#' yield draws from a distribution for the quantities of interest.
#' An enormously useful feature of draws from a distribution is
#' that they can used to generate distributions for new
#' quantities. Posterior distributions for death rates, for instance,
#' can be turned into posterior distributions for life expectancies.
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
#' - [draw_median()]
#' - [draw_mean()]
#' - [draw_mode()]
#' - [draw_quantile()]
#' 
#'
#' @section Other functions:
#'
#' [collapse_to_rvec]
#'
#' @section Design:
#'
#' `rvec` are built from [vctrs][vctrs::vctrs] package, which
#' look after many of the tricky details, and give it a sound
#' conceptual basis. Internally, many of the calculations are
#' done using the [matrixStats] package, is fast.
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
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
