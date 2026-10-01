# rvec 1.0.5

## New functions

- Added `extract_draws()` for selection by index, including repeated indices,
  and `thin_draws()` for random selection without replacement in original order.

- Added `draws_any_na()`, `draws_all_na()`, `draws_any_infinite()`,
  `draws_all_infinite()`, `draws_any_finite()`, and `draws_all_finite()`
  to check missingness and finiteness across draws for each element.

- `pmin()` and `pmax()` now accept rvecs in any argument position for
  elementwise bounds and comparisons within each draw. Calls without rvecs
  retain base R behaviour.

- `which.min()` and `which.max()` now find the first extreme position within
  each draw. Empty rvecs
  return empty index rvecs; nonempty draws with no valid index return `NA`
  with one summary warning per call. Ordinary inputs retain base R behaviour.

## Constructors

- `new_rvec_chr()`, `new_rvec_dbl()`, `new_rvec_int()`, and `new_rvec_lgl()`
  now accept a scalar `value` to fill all elements and draws, including `NA`.
  Existing defaults are unchanged.

## Summaries

- `draws_ci()` now accepts `point = "mean"` to report a mean point estimate;
  the default remains `point = "median"` and interval limits are unchanged.

- `min()`, `max()`, and `range()` now summarise elements independently
  within each draw, including multiple arguments and missing-value handling.

- `quantile()` now calculates quantiles independently within each draw,
  preserving base R's probability, missing-value, naming, and algorithm options.

## Bug fixes

- `rank()` now preserves fractional average ranks for tied values instead
  of failing when converting them to integers. Logical rvecs also use
  a compatible ranking method, and singleton and empty inputs retain
  their original draw counts.

## Documentation

- Improved documentation for draw summaries and resolved a roxygen warning
  about matrix multiplication methods while retaining support for R < 4.3.

## Clarifying interface

- Internal functions now enforce the (previously implicit) constraint
  that rvecs must have at least one draw. Rvecs of length 0 (ie with 1+ columns
  but 0 rows internally) continue to be allowed.


# rvec 1.0.4

## Memory use

- Reduced temporary memory use in constructors, casts, arithmetic, comparisons,
  logical math, summaries, covariance, weighted summaries, `if_else_rvec()`,
  `draws_mode()`, draw pooling, and expansion by avoiding unnecessary copies
  and repeated data, while preserving existing behavior.

## Bug fixes

- Weighted summaries correctly align a one-draw rvec with a multi-draw rvec,
  whether the one-draw input supplies values or weights. Previously these
  calls failed with a subscript-out-of-bounds error.

# rvec 1.0.3

## Memory use

- Distribution functions use less temporary memory by retaining shared
  parameters in compact form and avoiding unnecessary input and output
  matrix copies. The changes cover all distribution families supported by
  rvec and preserve results, warnings, and random-number generator behavior,
  apart from the bug fixes below. Random output remains double-valued.

- Calculations continue to use the same base R distribution functions,
  including the distinction between omitted and explicitly supplied `ncp`.
  Negative binomial conversion from `mu` to `prob` avoids intermediate
  rvecs while retaining the same arithmetic.

- Multinomial functions retain compact inputs and the existing order of
  base R calls. `rmultinom_rvec()` allocates double output directly, and
  `dmultinom_rvec()` avoids an unnecessary copy when calculating default
  size for standard double rvecs.

## Bug fixes

- Hypergeometric density, probability, and quantile functions correctly name
  `n` and `k` when their draw counts are incompatible; the error previously
  named `k` twice.

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
