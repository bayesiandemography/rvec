
# rvec 1.0.3

## Memory use

- `rgamma_rvec()` uses less temporary memory without processing draws in
  chunks. After checking rvec alignment rules, it lets base R recycle
  parameters across draws instead of constructing full-sized repeated
  copies. It also avoids unnecessary copies of parameter matrices and
  assigns output dimensions directly to the generated values. It continues
  to use base R's `rgamma()`, preserving results for a given random seed.

- `rpois_rvec()` uses the same approach to avoid expanding parameters
  across draws and copying the output matrix. Results and random-number
  generator state are preserved, and output remains double-valued.

- `dpois_rvec()`, `ppois_rvec()`, and `qpois_rvec()` also avoid expanding
  arguments across draws and copying parameter and output matrices, while
  preserving input alignment and results.

- `dgamma_rvec()`, `pgamma_rvec()`, and `qgamma_rvec()` use compact arguments
  and avoid unnecessary matrix copies, preserving alignment across their
  three arguments and the existing handling of rate and scale.

- `dnorm_rvec()`, `pnorm_rvec()`, `qnorm_rvec()`, and `rnorm_rvec()` use
  compact arguments and avoid unnecessary matrix copies. Results and
  random-number generator state are preserved.

- `dlnorm_rvec()`, `plnorm_rvec()`, `qlnorm_rvec()`, and `rlnorm_rvec()` use
  the same compact-argument approach, reducing temporary memory while
  preserving results and random-number generator state.

- `dbinom_rvec()`, `pbinom_rvec()`, `qbinom_rvec()`, and `rbinom_rvec()` use
  compact arguments and avoid unnecessary matrix copies. Results and
  random-number generator state are preserved, including double-valued
  output from `rbinom_rvec()`.

- The exponential and geometric distribution functions also use compact
  arguments and avoid unnecessary matrix copies. Results, warnings, and
  random-number generator state are preserved; `rgeom_rvec()` continues
  to return doubles.

- `dcauchy_rvec()`, `pcauchy_rvec()`, `qcauchy_rvec()`, and `rcauchy_rvec()`
  use compact arguments and avoid unnecessary matrix copies, preserving
  results and random-number generator state.

- `dunif_rvec()`, `punif_rvec()`, `qunif_rvec()`, and `runif_rvec()` use
  compact arguments and avoid unnecessary matrix copies, preserving
  results and random-number generator state.

- `dweibull_rvec()`, `pweibull_rvec()`, `qweibull_rvec()`, and `rweibull_rvec()`
  use compact arguments and avoid unnecessary matrix copies, preserving
  results and random-number generator state.

- `dchisq_rvec()`, `pchisq_rvec()`, `qchisq_rvec()`, and `rchisq_rvec()`
  use compact arguments and avoid unnecessary matrix copies, preserving
  results and random-number generator state. The distinction between
  omitted `ncp` and explicitly supplied zero is retained.

- `dt_rvec()`, `pt_rvec()`, `qt_rvec()`, and `rt_rvec()` use compact
  arguments and avoid unnecessary matrix copies, preserving results and
  random-number generator state. Omitted `ncp` remains distinct from
  explicitly supplied zero, and noncentral draws still use base R's
  calculation.

- `dbeta_rvec()`, `pbeta_rvec()`, `qbeta_rvec()`, and `rbeta_rvec()` use
  compact arguments and avoid unnecessary matrix copies, preserving
  results and random-number generator state. Omitted `ncp` remains
  distinct from explicitly supplied zero, and noncentral draws still
  use base R's calculation.

## Bug fixes

- Random-generation functions such as `rgamma_rvec()` now correctly recycle
  single-draw rvec parameters when another parameter has multiple draws and
  `n_draw` is omitted. Previously, if the first rvec parameter had a single
  draw, the call failed with an internal length error after advancing the
  random-number generator state.


# rvec 1.0.2

## covr

- Results from coverage tests not being uploaded to covr site,
  so updating yaml.



# rvec 1.0.1

## Reducing minimum R version

- The minimum R version has been reduced from 4.3.0 to 4.2.0. However,
  it does not appear to be possible to safely implement matrix
  multiplication without `matrixOps`, which was introduced in 4.3.0. 
  Methods for matrix multiplication are therefore only implemented if
  R >= 4.3.0.


# rvec 1.0.0

## Change to lifecycle status

- Interface is now sufficiently stable that the "experimental"
  lifecycle badge has been removed.
  
## Changes to interface

- Added functions `draws_sd()`, `draws_var()`, `draws_cv()` for
  summarising across draws. (#37)
- Added function `pool_draws()`, for combining draws across
  categories. (#35)
- Added functions `new_rvec_chr()`, `new_rvec_dbl()`,
  `new_rvec_int()`, and `new_rvec_lgl()`. Deprecated function
  `new_rvec()`. The new functions initialise a vector with 0, "", or
  `FALSE`, while `new_rvec()` initialised it with `NA`, which was
  awkward. (#36)
- Added quotation marks to printed rvec_chr objects.
- Added `%*%` method for `Matrix::Matrix` objects. (#31)

## Documentation

- Removed warning about r* functions returning doubles. (#28)

# rvec 0.0.8

## Changes to interface

- Added function `prob()`, a version of `draws_mean()` that works only
  with logical rvecs. (#27)
- `rvec()` and `rvec_dbl()` now accept sparse matrices (inheriting
  from "Matrix"), in addition to dense matrices. (#25)
- Function `rbinom_rvec()`, `rgeom_rvec()`, `rhyper_rvec()`,
  `rmultinom_rvec()`, `rnbinom_rvec()`, and `rpois_rvec()` now always
  return doubles, even when the counts are small. The standard R
  approach of giving integers when counts are small and doubles when
  counts are large was generating Valgrind errors in dependent
  packages.
  

# rvec 0.0.7

## Changes to interface

- Removed `is.numeric` methods for rvecs. These had been creating
  problems with functions from non-rvec packages, since `is.numeric`
  generally implies that an object is a base R style numeric vector.
- Removed space from around `=` when printing `rvec_lgl`, so that, for
  instance, `p = 0.5` becomes `p=0.5`.
- `rvec()`, `rvec_chr()`, `rvec_dbl()`, `rvec_int()`, and
  `rvec_lgl()` now accept rvec arguments.
- `draws_ci()` now accepts `width` arguments with length greater than
  1.
- Improved error messages from distribution functions.

  
## New functions

- Added function `new_rvec()`, which creates rvecs with specified
  values for type, length, and `n_draw`, consisting entirely of NAs.
- Added function `extract_draw()`, which extracts a single
  draw from an rvec.


# rvec 0.0.6

## Documentation

- Fixed typo in DESCRIPTION
- Added 'value' section to documentation for "missing"
- Added examples to documentation for "missing"

## Interface

- Changed `anyNA()` so it returns an rvec,
  rather than a logical scalar.


# rvec 0.0.5

## Features

- added default case to n_draw

## Documentation

- sundry tidying of help files


# rvec 0.0.4

## Documentation

- Export generices for sd, var, rank, and add documentation

## Internals

- Change argument names for matrixOps to 'x' and 'y'


# rvec 0.0.3

## Documentation

- Split help for distributions into multiple files
- Revise vignette

## Features

- added 'by' argument to collapse_to_rvec
- added summary method
- added 'rank', 'order', 'sort'


# rvec 0.0.2

## Bug fix

- Added `drop = FALSE` argument to calls to `matrixStats::rowQuantiles()`

# rvec 0.0.1

## Minor feature added

- Added method for `is.numeric()`. (Can't add methods for 
`is.character()`, `is.double()`, `is.integer()`, `is.logical()`, 
since these are non-generic primitives.
