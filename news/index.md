# Changelog

## rvec 1.0.5

### New functions

- Added
  [`extract_draws()`](https://bayesiandemography.github.io/rvec/reference/extract_draws.md)
  for selection by index, including repeated indices, and
  [`thin_draws()`](https://bayesiandemography.github.io/rvec/reference/thin_draws.md)
  for random selection without replacement in original order.

- Added
  [`draws_any_na()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md),
  [`draws_all_na()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md),
  [`draws_any_infinite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md),
  [`draws_all_infinite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md),
  [`draws_any_finite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md),
  and
  [`draws_all_finite()`](https://bayesiandemography.github.io/rvec/reference/draws_any_na.md)
  to check missingness and finiteness across draws for each element.

- [`pmin()`](https://bayesiandemography.github.io/rvec/reference/pmin.md)
  and
  [`pmax()`](https://bayesiandemography.github.io/rvec/reference/pmin.md)
  now accept rvecs in any argument position for elementwise bounds and
  comparisons within each draw. Calls without rvecs retain base R
  behaviour.

- [`which.min()`](https://bayesiandemography.github.io/rvec/reference/which.min.md)
  and
  [`which.max()`](https://bayesiandemography.github.io/rvec/reference/which.min.md)
  now find the first extreme position within each draw. Empty rvecs
  return empty index rvecs; nonempty draws with no valid index return
  `NA` with one summary warning per call. Ordinary inputs retain base R
  behaviour.

### Constructors

- [`new_rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_int()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  and
  [`new_rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md)
  now accept a scalar `value` to fill all elements and draws, including
  `NA`. Existing defaults are unchanged.

### Summaries

- [`draws_ci()`](https://bayesiandemography.github.io/rvec/reference/draws_ci.md)
  now accepts `point = "mean"` to report a mean point estimate; the
  default remains `point = "median"` and interval limits are unchanged.

- [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html), and
  [`range()`](https://rdrr.io/r/base/range.html) now summarise elements
  independently within each draw, including multiple arguments and
  missing-value handling.

- [`quantile()`](https://rdrr.io/r/stats/quantile.html) now calculates
  quantiles independently within each draw, preserving base R’s
  probability, missing-value, naming, and algorithm options.

### Bug fixes

- [`rank()`](https://bayesiandemography.github.io/rvec/reference/rank.md)
  now preserves fractional average ranks for tied values instead of
  failing when converting them to integers. Logical rvecs also use a
  compatible ranking method, and singleton and empty inputs retain their
  original draw counts.

### Documentation

- Improved documentation for draw summaries and resolved a roxygen
  warning about matrix multiplication methods while retaining support
  for R \< 4.3.

### Clarifying interface

- Internal functions now enforce the (previously implicit) constraint
  that rvecs must have at least one draw. Rvecs of length 0 (ie with 1+
  columns but 0 rows internally) continue to be allowed.

## rvec 1.0.4

### Memory use

- Reduced temporary memory use in constructors, casts, arithmetic,
  comparisons, logical math, summaries, covariance, weighted summaries,
  [`if_else_rvec()`](https://bayesiandemography.github.io/rvec/reference/if_else_rvec.md),
  [`draws_mode()`](https://bayesiandemography.github.io/rvec/reference/draws_median.md),
  draw pooling, and expansion by avoiding unnecessary copies and
  repeated data, while preserving existing behavior.

### Bug fixes

- Weighted summaries correctly align a one-draw rvec with a multi-draw
  rvec, whether the one-draw input supplies values or weights.
  Previously these calls failed with a subscript-out-of-bounds error.

## rvec 1.0.3

### Memory use

- Distribution functions use less temporary memory by retaining shared
  parameters in compact form and avoiding unnecessary input and output
  matrix copies. The changes cover all distribution families supported
  by rvec and preserve results, warnings, and random-number generator
  behavior, apart from the bug fixes below. Random output remains
  double-valued.

- Calculations continue to use the same base R distribution functions,
  including the distinction between omitted and explicitly supplied
  `ncp`. Negative binomial conversion from `mu` to `prob` avoids
  intermediate rvecs while retaining the same arithmetic.

- Multinomial functions retain compact inputs and the existing order of
  base R calls.
  [`rmultinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dmultinom_rvec.md)
  allocates double output directly, and
  [`dmultinom_rvec()`](https://bayesiandemography.github.io/rvec/reference/dmultinom_rvec.md)
  avoids an unnecessary copy when calculating default size for standard
  double rvecs.

### Bug fixes

- Hypergeometric density, probability, and quantile functions correctly
  name `n` and `k` when their draw counts are incompatible; the error
  previously named `k` twice.

- Random-generation functions such as
  [`rgamma_rvec()`](https://bayesiandemography.github.io/rvec/reference/dgamma_rvec.md)
  now correctly recycle single-draw rvec parameters when another
  parameter has multiple draws and `n_draw` is omitted. Previously, if
  the first rvec parameter had a single draw, the call failed with an
  internal length error after advancing the random-number generator
  state.

## rvec 1.0.2

### covr

- Results from coverage tests not being uploaded to covr site, so
  updating yaml.

## rvec 1.0.1

CRAN release: 2026-02-15

### Reducing minimum R version

- The minimum R version has been reduced from 4.3.0 to 4.2.0. However,
  it does not appear to be possible to safely implement matrix
  multiplication without `matrixOps`, which was introduced in 4.3.0.
  Methods for matrix multiplication are therefore only implemented if R
  \>= 4.3.0.

## rvec 1.0.0

CRAN release: 2025-12-10

### Change to lifecycle status

- Interface is now sufficiently stable that the “experimental” lifecycle
  badge has been removed.

### Changes to interface

- Added functions
  [`draws_sd()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md),
  [`draws_var()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md),
  [`draws_cv()`](https://bayesiandemography.github.io/rvec/reference/draws_sd.md)
  for summarising across draws.
  ([\#37](https://github.com/bayesiandemography/rvec/issues/37))
- Added function
  [`pool_draws()`](https://bayesiandemography.github.io/rvec/reference/pool_draws.md),
  for combining draws across categories.
  ([\#35](https://github.com/bayesiandemography/rvec/issues/35))
- Added functions
  [`new_rvec_chr()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_dbl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  [`new_rvec_int()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md),
  and
  [`new_rvec_lgl()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_blank.md).
  Deprecated function
  [`new_rvec()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_deprecated.md).
  The new functions initialise a vector with 0, ““, or `FALSE`, while
  [`new_rvec()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_deprecated.md)
  initialised it with `NA`, which was awkward.
  ([\#36](https://github.com/bayesiandemography/rvec/issues/36))
- Added quotation marks to printed rvec_chr objects.
- Added `%*%` method for
  [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  objects.
  ([\#31](https://github.com/bayesiandemography/rvec/issues/31))

### Documentation

- Removed warning about r\* functions returning doubles.
  ([\#28](https://github.com/bayesiandemography/rvec/issues/28))

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
  [`new_rvec()`](https://bayesiandemography.github.io/rvec/reference/new_rvec_deprecated.md),
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
