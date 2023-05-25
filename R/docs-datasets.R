

#' Divorce rates in New Zealand
#'
#' Posterior sample from a model of divorce rates
#' in New Zealand.
#'
#' @format A tibble with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+.
#' - `sex`: `"Female"` or `"Male"`.
#' - `time`: Calendar year.
#' - `sim`: Index for draw.
#' - `rate`: Divorce rate.
#'
#' @source Derived from data in tables "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)" and
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website.
#' Divorce data downloaded on 22 March 2023, and
#' population data downloaded on 26 March 2023.
"divorce"


#' Regression coefficients
#'
#' Posterior sample for coefficients from a linear
#' regression model.
#'
#' @format A tibble with 200 rows and
#' the following variables:
#' - `draw`: Index identifying random draw.
#' - `alpha`: Intercept parameter
#' - `beta`: Slope parameter
#' - `sigma`: Standard deviation of error term
#'
#' @source `regr_coef` contains values from the second
#' half of the `line` dataset
#' in package [coda](https://CRAN.R-project.org/package=coda).
#' The line dataset draws on the BUGS manual:
#' Spiegelhalter, D.J., Thomas, A., Best, N.G. and
#' Gilks, W.R. (1995) BUGS: Bayesian inference using
#' Gibbs Sampling, Version 0.5, MRC Biostatistics Unit,
#' Cambridge.
"regr_coef"



