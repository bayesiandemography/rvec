# rvec 1.0.4

## Memory use

- `sum()`, `prod()`, `any()`, and `all()` avoid concatenating a single
  unnamed numeric or logical rvec before summarising it. Named inputs,
  multiple inputs, and custom subclasses retain the existing dispatch.

- Mathematical operations on logical rvecs convert their data to integer
  with fewer temporary copies, preserving dimensions, row names, and result
  types.

- `is.nan()`, `is.finite()`, and `is.infinite()` operate directly on logical
  rvec data, avoiding temporary copies used to convert it to integer data.

- Weighted means, medians, MADs, variances, and standard deviations retain
  ordinary `x` inputs in compact form when weights are rvecs, instead of
  repeating `x` across all draws.

- Covariance calculations process rvec matrix columns one at a time instead
  of retaining lists containing copies of every column.

- `if_else_rvec()` retains ordinary and one-draw `false` and `missing`
  branches in compact form instead of expanding each one across all draws.

- Comparisons retain shared operands in compact form rather than repeating
  them across draws. Draw summaries, standard deviations, and variances
  avoid coercion copies when inputs are already double-valued.

- Typed constructors and same-type casts reuse suitable matrices when the
  type and draw count already match, avoiding unnecessary copies while
  preserving row names and independent modification of inputs and outputs.

- Binary arithmetic avoids unnecessary input and output matrix copies and
  retains single-draw operands in compact form. Observation recycling,
  result types, names, and integer-overflow behavior are preserved.

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
