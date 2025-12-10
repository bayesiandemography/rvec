# Changelog

## rvec 0.0.8

CRAN release: 2025-07-13

### Changes to interface

- Added function
  [`prob()`](https://bayesiandemography.github.io/rvec/reference/prob.md),
  a version of
  [`draws_mean()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md)
  that works only with logical rvecs.
  ([\#27](https://github.com/bayesiandemography/rvec/issues/27))
- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  and
  [`rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  now accept sparse matrices (inheriting from “Matrix”), in addition to
  dense matrices.
  ([\#25](https://github.com/bayesiandemography/rvec/issues/25))
- Function
  [`rbinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dbinom_rvec.md),
  [`rgeom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dgeom_rvec.md),
  [`rhyper_rvec()`](https://bayesiandemography.github.io/rvec/reference/dhyper_rvec.md),
  [`rmultinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dmultinom_rvec.md),
  [`rnbinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dnbinom_rvec.md),
  and
  [`rpois_rvec()`](https://bayesiandemography.github.io/rvec/reference/dpois_rvec.md)
  now always return doubles, even when the counts are small. The
  standard R approach of giving integers when counts are small and
  doubles when counts are large was generating Valgrind errors in
  dependent packages.

## rvec 0.0.7

CRAN release: 2024-09-15

### Changes to interface

- Removed `is.numeric` methods for rvecs. These had been creating
  problems with functions from non-rvec packages, since `is.numeric`
  generally implies that an object is a base R style numeric vector.
- Removed space from around `=` when printing `rvec_lgl`, so that, for
  instance, `p = 0.5` becomes `p=0.5`.
- [`rvec()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  [`rvec_int()`](https://bayesiandemography.github.io/rvec/reference/rvec.md),
  and
  [`rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/rvec.md)
  now accept rvec arguments.
- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)
  now accepts `width` arguments with length greater than
  1.  
- Improved error messages from distribution functions.

### New functions

- Added function
  [`new_rvec()`](https://bayesiandemography.github.io/rvec/reference/new_rvec.md),
  which creates rvecs with specified values for type, length, and
  `n_draw`, consisting entirely of NAs.
- Added function
  [`extract_draw()`](https://bayesiandemography.github.io/rvec/reference/extract_draw.md),
  which extracts a single draw from an rvec.

## rvec 0.0.6

CRAN release: 2023-11-08

### Documentation

- Fixed typo in DESCRIPTION
- Added ‘value’ section to documentation for “missing”
- Added examples to documentation for “missing”

### Interface

- Changed [`anyNA()`](https://rdrr.io/r/base/NA.html) so it returns an
  rvec, rather than a logical scalar.

## rvec 0.0.5

### Features

- added default case to n_draw

### Documentation

- sundry tidying of help files

## rvec 0.0.4

### Documentation

- Export generices for sd, var, rank, and add documentation

### Internals

- Change argument names for matrixOps to ‘x’ and ‘y’

## rvec 0.0.3

### Documentation

- Split help for distributions into multiple files
- Revise vignette

### Features

- added ‘by’ argument to collapse_to_rvec
- added summary method
- added ‘rank’, ‘order’, ‘sort’

## rvec 0.0.2

### Bug fix

- Added `drop = FALSE` argument to calls to
  [`matrixStats::rowQuantiles()`](https://rdrr.io/pkg/matrixStats/man/rowQuantiles.html)

## rvec 0.0.1

### Minor feature added

- Added method for
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html). (Can’t add
  methods for [`is.character()`](https://rdrr.io/r/base/character.html),
  [`is.double()`](https://rdrr.io/r/base/double.html),
  [`is.integer()`](https://rdrr.io/r/base/integer.html),
  [`is.logical()`](https://rdrr.io/r/base/logical.html), since these are
  non-generic primitives.
