

#' Divorce Rates in New Zealand
#'
#' Posterior sample from a model of divorce rates
#' in New Zealand.
#'
#' @format A tibble with 30,000 rows and the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+.
#' - `sex`: `"Female"` or `"Male"`.
#' - `draw`: Index for random draw.
#' - `rate`: Divorce rate, per 1000.
#'
#' @source Derived from data in tables "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)" and
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website,
#' downloaded on 22 March 2023.
"divorce"


#' Posterior Sample from Linear Regression
#'
#' Posterior sample for parameters from a linear
#' regression model.
#'
#' @format A matrix with 200 columns and
#' the following rows:
#' - `alpha`: Intercept parameter
#' - `beta`: Slope parameter
#' - `sigma`: Standard deviation of error term
#'
#' @source `reg_post` contains values from the second
#' half of the `line` dataset
#' in package [coda](https://CRAN.R-project.org/package=coda).
#' The line dataset draws on the BUGS manual:
#' Spiegelhalter, D.J., Thomas, A., Best, N.G. and
#' Gilks, W.R. (1995) BUGS: Bayesian inference using
#' Gibbs Sampling, Version 0.5, MRC Biostatistics Unit,
#' Cambridge.
"reg_post"



