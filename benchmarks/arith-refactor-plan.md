# Arithmetic memory refactor: findings and plan

## Status and scope

Work branch: `arith-refactor`, created from `dev` at `31b966f`.
The distribution refactor is complete. This document records the subsequent
read-only review of `R/vec_arith.R` and a temporary binary-arithmetic experiment.
No arithmetic package code has been changed yet.

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
