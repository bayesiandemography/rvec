
# rvec 0.0.8

## Changes to interface

- Added function `prob()`, a version of `draws_mean()` that works only
  with logical rvecs.
  

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


