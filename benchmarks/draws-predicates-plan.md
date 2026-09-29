# Draw summaries for missing, finite, and infinite values

## Purpose and scope

Add six explicit draw summaries that reduce the effort needed to ask common
diagnostic questions correctly. Discoverability and clarity about the direction
of summarisation are the primary reasons for this API; memory efficiency is a
secondary implementation goal.

- `draws_any_na(x)`
- `draws_all_na(x)`
- `draws_any_infinite(x)`
- `draws_all_infinite(x)`
- `draws_any_finite(x)`
- `draws_all_finite(x)`

This document records the implementation plan and completion results below. Do not add counts, proportions,
NaN-only summaries, or change existing summary behaviour as part of this work.
The plan lives alongside the existing refactor plan in `benchmarks/`, which is
already excluded from the built package.

## Proposed API and semantics

Use S3 generics with the single argument `x`, following the existing `draws_*`
family. Return an ordinary logical vector of length `length(x)`, preserving
element names. Each result describes the draws belonging to one element.
Results never contain `NA`; there is no `na_rm` argument.

| Predicate | TRUE for | FALSE for |
| --- | --- | --- |
| na | NA and NaN | All other values |
| infinite | Inf and -Inf | Finite values, NA, and NaN |
| finite | Finite numeric and nonmissing logical values | Inf, -Inf, NA, and NaN |

Apply `any` or `all` to the relevant predicate across draws. In particular,
`draws_all_finite(x)` is not the complement of `draws_any_infinite(x)` when
missing values occur.

Support missingness summaries for all four rvec types, including character.
Support finiteness summaries for double, integer, and logical rvecs. Proposed
character methods for finiteness raise a clear error: classifying strings as
finite or infinite would be misleading. Document this restriction explicitly.

For a zero-length rvec, return `logical(0)` with names preserved where present.
This follows the one-result-per-element contract rather than copying the
existing scalar empty-input behaviour of `draws_any()` and `draws_all()`.
Leave those existing functions unchanged. Inspect constructor invariants before
testing zero-draw inputs: if supported, each existing element has `any = FALSE`
and `all = TRUE`; do not broaden valid rvec representations for these functions.

## Implementation

1. Inspect existing draw summaries, predicate methods, constructors, and naming
   tests. Confirm supported empty shapes and subclass conventions.
2. Add the functions and methods in `R/draws.R`, sharing small internal helpers
   where they reduce duplication without obscuring the six public operations.
3. Prefer existing matrixStats row operations where their semantics and type
   support match the contract, especially `rowAnyNAs()` for missingness. Check
   compatibility with the package's supported dependency versions.
4. For operations without a suitable primitive, compare a straightforward
   predicate-plus-row-summary implementation with bounded-memory traversal.
   Avoid copying the entire input or coercing logical/integer inputs to double.
   Do not add compiled code or dependencies merely for these conveniences.
5. Preserve names and input immutability, and handle empty inputs explicitly.
   Follow existing S3 conventions so subclasses can provide their own methods.

## Tests

Add focused tests, preferably in `tests/testthat/test-draws-predicates.R`.

- Check all six functions against independent row-wise base R calculations on
  the underlying matrix, using `any`/`all` and `is.na`/`is.infinite`/`is.finite`.
- Use a rectangular matrix with deliberately different rows and columns so
  accidental summarisation over elements instead of draws cannot pass.
- Cover mixtures of finite values, NA, NaN, Inf, and -Inf; all-missing,
  all-infinite, all-finite, and mixed rows; and both signs of infinity.
- Cover double, integer, logical, and character inputs according to the type
  contract, including typed missing values and character errors.
- Assert logical output type, exact length, names, absence of missing results,
  one-element and one-draw cases, empty inputs, and input immutability.
- For nonempty supported inputs, verify equivalence to compositions such as
  `draws_any(is.na(x))` and `draws_all(is.finite(x))`.
- Include an explicit case showing that no infinite draws does not imply all
  finite draws when NA or NaN is present.
- Check S3 dispatch with a small test subclass if new helper structure makes
  dispatch behaviour nontrivial.

## Documentation

Use a shared help topic explaining the six questions in everyday language.
Include the predicate truth table, return shape, type restrictions, empty-input
behaviour, and the treatment of NaN. Provide a small named example and a data
filtering example using `draws_all_finite()` or `draws_any_na()`.

Explicitly distinguish `anyNA(x)`, which returns a length-one rvec summarising
elements within each draw, from `draws_any_na(x)`, which returns one ordinary
logical value per element by summarising its draws. Also distinguish predicates
such as `is.na(x)`, which preserve all draws, from the new summaries.

Update relevant links in the missing-value help, existing logical draw-summary
help, and package overview. Add a concise NEWS entry under the current version.
Regenerate Rd files and NAMESPACE with the project's documentation workflow and
review the generated diff for unrelated changes.

## Validation and completion

Run focused tests first, then the full suite and package checks. Verify examples
and generated documentation. Compare representative large-matrix allocations
and runtime with the existing composed expressions before claiming performance
improvements; correctness and a simple implementation take priority.

Review the final diff for API consistency and accidental changes to existing
functions. Report results and any limitations. Commit, merge, and push only when
requested; branch creation and this plan are the currently authorised work.


## Completion results

Implemented the six S3 generics and methods, shared documentation, cross-links,
NEWS entry, and 176 new assertions. Public constructors reject zero-draw
matrices; no representation changes were needed. Empty rvecs return a logical
vector of length zero, and finiteness checks reject character inputs.

Missingness uses `matrixStats::rowAnyNAs()` and `rowAlls(value = NA)` directly.
Finiteness uses a predicate matrix and `rowAnys()` or `rowAlls()`, without an
intermediate rvec. A column-traversal prototype was rejected: for a 2,000-element,
1,000-draw example its measured vector-heap growth was about 40 MB, compared
with about 8 MB for the composed expression. The retained implementation also
used about 8 MB. Results are in `results/draws-predicates.csv`, reproducible with
`draws-predicates.R`; timings of a few milliseconds are illustrative, not a
reliable speed comparison. No general memory or speed improvement is claimed.
The existing package already uses `colAnyNAs()`; its row counterpart was
introduced in the same matrixStats release (0.52.0).

Validation: all 11,892 assertions passed, with no test warnings or failures.
R CMD check, including examples and vignettes, completed with zero errors,
zero warnings, and one environment note: unable to verify current time.
Generated documentation was reviewed and unrelated changes from the installed
newer roxygen2 were removed. Existing summary behaviour is unchanged.
