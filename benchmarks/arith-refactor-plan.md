# Arithmetic memory refactor: findings and plan

## Status and scope

Work branch: `arith-refactor`, created from `dev` at `31b966f`.
The distribution refactor is complete. This document records the subsequent
read-only review of `R/vec_arith.R` and a temporary binary-arithmetic experiment.
At the time of the initial plan, no arithmetic package code had been changed.
See the implementation record below for current status.

The proposed scope is binary arithmetic between double, integer, and logical
rvecs, and between those rvecs and ordinary double, integer, or logical vectors.
The operators tested so far are `+`, `-`, `*`, `/`, `^`, `%%`, and `%/%`.
Unary operations are outside the initial scope.

## Findings from the code review

### Input reconstruction

The nine rvec–rvec methods first validate draw compatibility with
`n_draw_common()`, then call a type-specific `rvec_to_rvec_*()` helper on each
operand. These conversions flatten and reconstruct the underlying matrices,
even when the input types and draw counts already match. The existing matrices
can potentially be used directly in that case.

### Single-draw expansion

The conversion helpers also expand single-draw inputs to the common draw count.
A compact vector can instead be reused across columns during base arithmetic.
Observation recycling must happen separately: a one-observation rvec with
multiple draws must repeat each draw down its corresponding column. Flattening
both original inputs before resolving observation counts would be incorrect.

### Output reconstruction

Methods involving doubles generally finish with `rvec_dbl(data)`. Its matrix
path flattens, casts, and reconstructs the result, even when the arithmetic
already produced a double matrix. Internal constructors could wrap an already
validated matrix directly. Result type must still follow current behavior;
integer and logical arithmetic must not be universally converted to doubles.

### Shared implementation

The methods repeat much of the same logic. A private helper could handle
alignment and arithmetic while keeping the existing S3 entry points and their
result-type rules. Changes to general-purpose constructors or conversion
helpers are not needed for the initial refactor.

## Temporary experiment

The experiment is currently in `/tmp/rvec-arith-experiment/run.R`. This is an
exploratory artifact and may disappear; the description below records its
essential design. It loads the current package and defines a separate prototype
without replacing package methods.

The combined prototype:

1. Checks draw compatibility using `n_draw_common()`.
2. Reads the underlying matrices without type-preserving reconstruction.
3. For differing draw counts, recycles observations with
   `vctrs::vec_recycle_common()` before converting the one-column operand to a
   plain vector. It then applies the requested base operator, restores output
   dimensions, and preserves the existing row-name precedence.
4. For other layouts, uses `vctrs::vec_arith_base()` on the existing data.
5. Wraps the result using an internal constructor when its type is already
   suitable, retaining the existing conversion path when needed.

Intermediate variants retained ordinary output construction or retained
single-draw expansion to help separate the sources of memory savings. Exact
compatibility comparisons were run on the combined prototype; intermediate
variants have not received equivalent validation.

### Memory measurements

Each measurement ran in a fresh R process. Inputs were constructed before
measurement, garbage-collection high-water marks were reset, and the change in
maximum Vcell use was recorded after one addition. Inputs had 1,000 observations
and 1,000 output draws. Each result occupied about 8 MB.

| Addition inputs | Current peak vector-heap growth | Combined prototype |
| --- | ---: | ---: |
| Two full-sized double rvecs | 48.1 MB | 9.0 MB |
| Full double rvec and single-draw double rvec | 40.1 MB | 9.0 MB |
| Full double rvec and ordinary double vector | 24.1 MB | 9.0 MB |

With two full-sized rvecs, avoiding input reconstruction alone reduced growth
to 25.1 MB; direct result construction reduced it further to about 9 MB.
Retaining compact single-draw inputs avoided another full-sized allocation.

These are decimal MB of R vector-heap growth, not process RSS or cumulative
allocation. Garbage-collection timing and prototype overhead affect the
figures. The measurements establish useful scope for improvement, not a
universal reduction or a speed guarantee.

### Compatibility comparisons

The combined prototype passed 5,600 comparisons over the seven binary operators
and double, integer, and logical inputs. Cases included ordinary scalar/vector
operands, one or two observations, one or three draws, named rvecs, integer
overflow, missing values, NaN, and infinities. Another 126 comparisons covered
empty inputs, incompatible observation/draw counts, operand reversal, and
conflicting or ordinary-vector names.

All 5,726 comparisons matched values and attributes, warning messages, and
error messages exactly. The experiment did not compare condition classes or
call stacks, did not cover custom subclasses, and did not test unary operations.
The comparison set is evidence for proceeding, not exhaustive validation.

## Proposed implementation plan

### 1. Establish a regression baseline

Preserve a reference to the pre-refactor implementation for temporary exact
comparisons. Add focused permanent tests for the public binary operations:

- All nine rvec type pairs and ordinary-vector operands in both orders.
- Matching draws, single-draw reuse, scalar observation recycling, and errors
  for incompatible observation or draw counts.
- Noncommutative operations in both operand orders.
- Result types, integer overflow, NA/NaN/Inf, division by zero, and warning
  behavior.
- Empty results, row names, attribute handling, and unchanged input objects.

Check relevant error classes as well as messages. Review subclass dispatch
before deciding whether any optimized path needs a conservative fallback.

### 2. Implement compact binary arithmetic

Introduce a private helper if it makes the repeated method bodies clearer.
Keep existing exported S3 method signatures and unsupported-operation behavior.
Resolve draw and observation compatibility in the same order as the existing
methods. Use underlying matrices directly when possible; keep single-draw
operands compact after observation recycling.

Use base operators on the original numeric types so integer overflow and
operator-specific coercion remain unchanged. Preserve operand order, output
dimensions, and row-name precedence explicitly where flattening is necessary.

### 3. Remove unnecessary result reconstruction

Wrap matrices with internal constructors only after establishing the required
type and attributes. Preserve the current double-result policy for methods
involving doubles and the operator-dependent types for other methods. Leave
unary methods and public constructor implementations unchanged initially.

### 4. Verify correctness and memory savings

Run targeted arithmetic tests and exact comparisons against the saved baseline.
Repeat representative memory measurements in fresh processes, including
ordinary vectors and both orientations of a single-draw operand. Expand the
measurements to integer/logical inputs and observation recycling. Add a
reproducible arithmetic benchmark alongside the distribution benchmark; record
source revisions and session details.

After focused checks pass, run the full package test suite, including the
distribution tests, and `R CMD check`. Record environmental limitations
separately from package failures. Add a concise NEWS entry for the measured
improvements and any explicitly agreed behavior changes.

### 5. Review and integrate

Review the diff and benchmark results before committing. Keep implementation
work on `arith-refactor`; merge or otherwise integrate into `dev` only when
requested. No push, merge, or release is part of the current planning step.


## Implementation record

The binary refactor was committed on `arith-refactor` as `d08797c`, including
the version bump to 1.0.4.
All binary S3 entry points delegate to a shared private helper. Standard numeric
rvecs use compact inputs; subclasses retain conversion dispatch. Unary methods
are unchanged. Regression tests cover type combinations, operators, recycling,
names, empty results, errors, input immutability, and subclass conversion.

- All 5,726 baseline comparisons match, including error classes, messages,
  warnings, result values, and attributes.
- The full test suite passes.
- `R CMD check` with `--no-manual` reports zero errors, zero warnings, and one
  environment-related note: remote current-time verification was unavailable.
- All 48 before/after arithmetic benchmark runs completed. For double addition,
  peak vector-heap growth fell from 48.1 to 8.1 MB for full inputs, 40.1 to
  8.0 MB for a single-draw operand, 24.1 to 8.1 MB for an ordinary vector, and
  40.1 to 16.1 MB for a one-observation rvec. Output size was about 8 MB.

The benchmark script, results, and session information are saved alongside the
distribution benchmarks. NEWS describes the change. The arithmetic changes are committed; nothing on this branch has been merged
or pushed. See the resumption plan below for subsequent work.


## Resumption plan (recorded after b23b6d4)

### Start here

- Current branch: `arith-refactor`. Latest implementation commit: `b23b6d4`.
- Package version: 1.0.4. NEWS already includes arithmetic and subsequent
  memory improvements; do not bump the version automatically for each step.
- `dev` and `origin/dev` were last updated through `31b966f`, completing the
  distribution refactor. The newer branch commits have not been pushed or merged.
- The working tree was clean before updating this document. Check its current
  state before resuming, preserving any intervening user work.
- Next recommended step: investigate and prototype compact branch handling in
  `if_else_rvec()`. Do not redo the completed arithmetic or distribution work.

### Completed after the binary-arithmetic refactor

Commit `b23b6d4` implements the first three groups from the package-wide review:

1. Typed constructors and same-type casts reuse suitable plain matrices when
   types and draw counts match. Row names are retained, column names are
   discarded as before, and unusual inputs use the original paths.
2. Comparisons retain compact operands while preserving common-type coercion,
   including character/numeric comparisons. Nonstandard inputs retain the
   original casting path.
3. `draws_median`, `draws_mean`, `draws_sd`, `draws_var`, `draws_cv`, `sd`, and
   single-input `var` avoid `1 * m` when the matrix is already double-valued.
   Integer/logical conversion paths remain unchanged.

Validation: 648 exact constructor/cast comparisons plus 9,224 comparison/summary
checks passed (9,872 total), including result attributes, warnings, and error
classes/messages. The full test suite passed. Package checking with
`--no-manual` reported zero errors, zero warnings, and one environment-related
note about remote time verification. The environment also printed diagnostics
for an incompatible aspell executable and restricted network/Quarto access;
these were not package-check warnings.

For 1,000 observations by 1,000 draws, constructor/cast peak vector-heap growth
fell from about 16 MB to under 0.2 MB, and comparison growth from about 28 MB to
12 MB. Same-type results share input storage until modification. Summary peak
heap measurements changed little, but allocation tracing of the methods
confirmed removal of the 8 MB coercion allocation; do not claim a measured
peak-memory reduction for summaries from these runs.

Saved evidence:

- `benchmarks/arithmetic.R` and `benchmarks/results/arithmetic*`.
- `benchmarks/common-operations.R` and `benchmarks/results/common*`.
- `benchmarks/README.md` documents commands, baselines, and metric limitations.
- Exploratory scripts are in `/tmp/rvec-arith-experiment` and
  `/tmp/rvec-memory-next`; these are disposable and may no longer exist.
  Reconstruct baselines from Git rather than relying on temporary files.

### Remaining candidates, in suggested order

1. **Mathematical functions and missingness (`R/vec_math.R`, `R/missing.R`).**
   Logical math first constructs an integer rvec. Investigate narrower paths
   that preserve types and overflow behavior. Reassess `is.na()` after the
   constructor fast paths: some of its previously identified copying may
   already be eliminated by `b23b6d4`.
2. **Summary dispatch (`sum`, `prod`, `any`, `all`).** The installed vctrs
   Summary method calls `vec_c(...)` even for one input. A single-input fast
   path may help, but preserve multi-argument behavior and dispatch. Reprofile
   after cast improvements before deciding whether this warrants new methods.
3. **Matrix multiplication (`R/matrixOps.R`).** The Matrix methods densify
   sparse operands, and rvec–rvec multiplication forms a full product before
   summing. Potentially large gains, but higher risk of changing accumulation,
   rounding, overflow, or dispatch. Treat as a separate investigation.
4. **Reshaping, pooling, modes, and formatting.** Inspect typed allocation in
   `collapse_to_rvec()`, `as.vector(t(m))` in expansion, reconstruction in
   pooling, and retention of all row frequency tables in mode/formatting
   paths. Distinguish unavoidable output allocations from avoidable temporaries.

### Conditional-selection implementation

The `if_else_rvec()` refactor is implemented in the working tree after
`16cb8f3`. It retains ordinary and one-draw `false` and `missing` branches in
compact form, selecting their values one draw at a time instead of constructing
full branch matrices. Assignments still process the complete `false` branch
before `missing`, including zero-length assignments, preserving the original
type-promotion behavior when a branch is unused.

Randomized comparison covered 6,912 combinations of output size, draw count,
branch type, and ordinary/one-draw/full layout with results identical to
`16cb8f3`. Fourteen focused checks and all 10,965 package tests passed. For
1,000 observations by 1,000 draws, peak vector-heap growth fell from about
41.3 MB to 34.1 MB for ordinary and one-draw branches; the full-rvec path
remained at about 45.4 MB. The reproducible benchmark and recorded comparison
are in `benchmarks/if-else.R` and `benchmarks/results/if-else*`.

### Covariance implementation

The covariance paths in `R/var.R` are implemented in the working tree after
`1521441`. They pass one matrix column at a time to `stats::var()` and
preallocate the result, avoiding lists that retained copies of every input
column. The non-rvec path preserves the existing behavior for matrix operands,
where each input draw can produce multiple output covariance values.

Comparison against `1521441` covered 1,040 combinations of empty and non-empty
inputs, logical/integer/double data, all five `use` modes, `na.rm`, missing and
non-finite values, and vector or matrix non-rvec operands. Results, warnings,
and error classes/messages were identical. For 1,000 observations by 1,000
draws, peak vector-heap growth fell from about 40.2 MB to 28.5 MB for two
rvecs, and from about 20.5 MB to 16.5 MB for an rvec and ordinary vector. The
reproducible benchmark and recorded comparison are in
`benchmarks/covariance.R` and `benchmarks/results/covariance*`. All 10,970
package tests passed. Package checking with `--no-manual` reported zero errors,
zero warnings, and the environment-related remote time verification note.

### Weighted-summary implementation

The weighted-summary changes are implemented in the working tree after
`8e9bf5e`. First, draw indexing was corrected to reuse column 1 for either
one-draw rvec operand. Both operand orders previously failed with a
subscript-out-of-bounds error; a regression test reproduced the failure before
the fix and now covers all five summaries. Separately, ordinary values are
kept in a single-column matrix rather than repeated across all weight draws.
Keeping the matrix conversion preserves the previous coercion behavior.

The memory change matched an alignment-only intermediate version in 7,200
comparisons covering empty, singleton, and longer inputs; ordinary, one-draw,
and full operands; logical/integer/double values; missing and infinite values;
zero weights; all five summaries; and both `na_rm` settings. Of these, 6,720
cases outside the intentional alignment fix also matched `8e9bf5e`. Another
30 comparisons of named, factor, Date, character, raw, and complex ordinary
inputs matched the baseline. Comparisons included results, warnings, and
error classes/messages. All 11,034 package test assertions passed. Package
checking with manual and vignette building disabled reported zero errors,
zero warnings, and one environment-related note about remote time verification.

For 1,000 observations by 1,000 draws, ordinary-value means and medians reduced
peak vector-heap growth from about 32.1 MB to 24.1 MB. MAD, variance, and
standard deviation stayed near 44.1 MB despite removing the repeated 8 MB
double matrix; garbage-collection timing affects this metric. Full-rvec cases
were essentially unchanged. Reproducible benchmarks and recorded comparisons
are in `benchmarks/weighted-summaries.R` and
`benchmarks/results/weighted-summaries*`.

Unary arithmetic was deliberately left unchanged and is not required to finish
these remaining candidates. General constructor conversions that change type
or draw count also remain on their original paths.

### Workflow for each next step

Keep changes small enough to review and commit separately. Inspect the current
code first, then prototype and measure before broad implementation. Preserve
exact results, types, attributes, warning/error behavior, and input immutability;
retain base numerical algorithms where possible. Establish whether previously
identified costs remain after shared-helper improvements.

Use focused permanent regression tests plus temporary comparisons against the
appropriate Git baseline. Measure in fresh processes with inputs outside the
measurement; report vector-heap growth accurately and distinguish it from
cumulative allocation and process RSS. Save reproducible benchmarks and update
NEWS. Run focused tests followed by full tests/package checks as appropriate.

User workflow so far: implement and report results, then commit when requested.
Do not merge, push, or release the branch without a separate instruction.
