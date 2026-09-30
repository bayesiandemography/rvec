# Plan: within-draw which.min(), which.max(), and is.unsorted()

## Purpose and restart context

Add operations across elements independently within each draw:

- `which.min(x)`: the position of the first minimum in each draw.
- `which.max(x)`: the position of the first maximum in each draw.
- `is.unsorted(x, na.rm = FALSE, strictly = FALSE)`: whether each draw is
  unsorted in the existing element order.

The user requested this saved plan so implementation could restart later.
Implementation was subsequently authorised and the edge-case policies below
were agreed during the restart discussion.

At the time of writing, the checkout is on `codex/parallel-extrema`, with
`8735986` (Add draw-wise pmin and pmax wrappers) committed and locally validated.
That branch has not yet been merged into dev or run through CI. Dev includes
within-draw min/max/range, rank fixes, and quantile; quantile passed all six CI
jobs after test-compatibility commit `12f60c4`. Inspect current Git state before
resuming; this description is a snapshot, not an instruction to reset branches.

The cookbook and troubleshooting vignettes are general collections intended
for future recipes. Use their Problem / Solution / Discussion / See Also style.

**Explicit user decision:** do not implement draw-wise `sort()` or `order()`,
now or as a future extension of this work. Their meaning is ambiguous and may
confuse users. Preserve existing single-draw support and multi-draw restrictions,
including `xtfrm.rvec`. An unsortedness check does not rearrange anything.

## Proposed semantics

### which.min() and which.max()

For nonempty inputs, return a length-one rvec with the input's number of
draws. For empty inputs, return a length-zero rvec retaining the draw count.
Positions refer to original element positions, starting at one. Ignore NA and
NaN as base R does; there is no `na.rm` argument. Ties select the first
occurrence, including ties at infinities. Logical values follow base
behaviour (FALSE before TRUE).

The agreed policies are:

1. **No valid position:** An empty rvec returns a zero-length integer rvec
   retaining its draw count, without a warning. For a nonempty rvec, each
   all-missing draw returns `NA_integer_`; warn once per call with the number
   of affected draws, including when all draws are missing. This is an
   intentional departure from base R's `integer(0)` for an all-missing vector.
2. **Character inputs:** base `which.min()` and `which.max()` coerce character
   values to numbers, potentially warning, rather than finding lexical extrema.
   Match base coercion and warnings. Do not accidentally implement lexical
   indices merely because min/max support character values.

Drop result names: the selected element (and its name) can differ
between draws, whereas rvec names are shared across draws. Retain integer
indices normally, but do not force a lossy integer cast if base returns double
indices for a long input.

Explain that these indices describe uncertain positions. They cannot generally
be used as an ordinary subscript to select one fixed row from a data frame.
Do not add draw-wise subsetting, label extraction, or a general `which()` here.

### is.unsorted()

Return a length-one logical rvec with one base-R result per draw, including NA
where base R returns it. Forward `na.rm` and `strictly` without changing their
meaning. Check numeric, logical, and character inputs. Character ordering must
follow the current locale, not a hard-coded lexical expectation.

Match base behaviour for empty and singleton draws, which return FALSE,
including a singleton NA. With more than one element, missing values normally
produce NA unless removed. With `strictly = TRUE`, equal adjacent values count
as unsorted. No decreasing-order option should be invented.

Drop result names, consistent with a summary across elements.

## Dispatch and implementation

1. Inspect base and vctrs implementations on supported R versions. In the local
   R 4.6.1 inspection, `which.min()` and `which.max()` call internal routines;
   `is.unsorted()` performs length and missingness checks before its internal
   call. Merely registering an S3 method may not intercept all these paths.
2. Prefer exported S3 generic wrappers with `.default` methods delegating to
   the base functions and `.rvec` methods for draw-wise calculations, following
   `rank()` in `R/order.R`. Verify this design before relying on it. Keep the
   original argument signatures and ordinary-input results, errors, warnings,
   and class-specific behaviour intact.
3. Document that attaching rvec masks these base names, and explicit `base::`
   calls bypass the wrappers. Use explicit base calls inside implementations
   to avoid recursion. Check for internal package calls affected by masking.
4. Use the underlying matrix columns without full-matrix expansion or transpose.
   Call the base operation per draw as the initial correctness-first approach.
   Preallocate where straightforward; do not introduce compiled code or new
   dependencies. Preserve the number of draws for empty and singleton inputs;
   avoid `apply()` simplification, which caused previous rank shape bugs.
5. Keep the implementation in a small focused file or a clearly separated
   section of `R/order.R`. Register exports/methods with roxygen and review
   NAMESPACE changes. Check both attached and clean installed-namespace use.

## Tests

Use independent column-wise base calculations as the oracle, applying only the
explicitly agreed adaptation for missing indices. Test:

- Draws with different selected positions and different sortedness outcomes,
  using nonsquare matrices so the wrong aggregation direction cannot pass.
- Ties, repeated infinities, NA versus NaN, all-missing draws, mixtures of valid
  and all-missing draws, and original positions after missing-value omission.
- Double, integer, logical, and the agreed character policy. Compare character
  behaviour with base on the test machine to avoid locale assumptions.
- Zero elements with several draws, one element with several draws, one draw
  with several elements, and named inputs. Assert exact dimensions, draw counts,
  storage types, and the agreed name policy, not just printed values.
- For is.unsorted, all combinations of na.rm and strictly, ascending and
  descending sequences, ties, and missing values at different positions.
- Ordinary-input equivalence, including empty inputs, classed inputs where
  supported by base, invalid arguments, warning behaviour, and namespace use.
- Input immutability and subclass dispatch for any new S3 generics.
- Existing sort/order/xtfrm restrictions remain unchanged. Do not weaken their
  tests or route them through a newly permissive ordering implementation.

## Documentation and validation

Exported wrappers need concise help explaining how they differ from the base
functions; unlike ordinary S3-only methods, they are new public entry points.
Document the missing-index and naming policies prominently.

Add a cookbook recipe identifying which region has the largest value in each
draw, and a recipe checking whether a sequence is increasing in every draw.
If useful, add troubleshooting for all-missing draws or treating random indices
as ordinary row selectors. Cross-link existing rank and summary documentation
and add NEWS under the then-current development version. Do not bump the
version automatically.

Run focused tests, the full test suite, and package checks including both recipe
vignettes. Inspect regenerated documentation and avoid unrelated roxygen churn.
Measure representative large inputs before making performance or memory claims.
Report any intentional departures from base behaviour and remaining limitations.
Commit, merge, push, and run the full six-job CI matrix only when authorised;
include R 4.2.2, current release, oldrel, devel, Windows, and macOS validation.
