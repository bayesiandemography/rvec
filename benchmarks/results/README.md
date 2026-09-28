# Recorded comparison

These runs used 1,000 observations/categories and 1,000 draws on the same
machine and R installation. The distribution sources were:

- Before: `0df93c8`, exported into a separate source directory.
- After: `0cbb332`, with only NEWS and benchmark files modified.

See the session files for R and dependency versions. These are example
measurements, not test thresholds. Some before/after processes overlapped;
do not use their timings to draw speed conclusions. Compare memory by matching
case and layout. Both runs completed all 20 cases.

At this scale, gamma and negative binomial cases show clear reductions, while
multinomial peak heap growth changes little. Larger multinomial cases expose
reductions more clearly, as explained in the parent README. R's garbage
collector makes peak growth sensitive to input size and the heap state.

## Arithmetic comparison

`arithmetic.csv` contains 48 measurements: before and after for four layouts,
three numeric types, and both operand orders. Before uses `0f1695e`; after uses
the uncommitted binary-arithmetic implementation on `arith-refactor` based on
that revision. Companion arithmetic session files record source checksums and
dependencies. The benchmark cases ran sequentially, alongside package checks;
timings are indicative only. All cases completed successfully.

## Common operations comparison

`common-operations.csv` records six cases before and after the constructors,
casts, comparisons, and double-input summary changes. Before uses `d08797c`;
after uses the uncommitted changes based on that revision on `arith-refactor`.
Session details are in `common-before-session.txt` and
`common-after-session.txt`. Each case uses a fresh process and collects input
setup garbage before resetting high-water marks. Same-type outputs may share
input storage; their object size is not additional allocated memory. All 12
cases completed successfully. Timings are indicative only.

## Conditional-selection comparison

`if-else.csv` records ordinary, one-draw, and full-rvec branch layouts before
and after the `if_else_rvec()` change. Before uses `16cb8f3`; after uses the
uncommitted implementation based on that revision. Session details are in
`if-else-before-session.txt` and `if-else-after-session.txt`.

For 1,000 observations by 1,000 draws, peak vector-heap growth fell from about
41.3 MB to 34.1 MB for both ordinary and one-draw `false` and `missing`
branches. The full-rvec case remained at about 45.4 MB. Timings are indicative
only.

## Covariance comparison

`covariance.csv` records covariance between two rvecs and between an rvec and
an ordinary vector. Before uses `1521441`; after uses the uncommitted
implementation based on that revision. Session details are in
`covariance-before-session.txt` and `covariance-after-session.txt`.

For 1,000 observations by 1,000 draws, peak vector-heap growth fell from about
40.2 MB to 28.5 MB for two rvecs and from about 20.5 MB to 16.5 MB for an rvec
and ordinary vector. Timings are indicative only.

## Weighted-summary comparison

`weighted-summaries.csv` records all five weighted summaries with rvec weights
and ordinary or full-rvec values. Before uses `8e9bf5e`; after uses the
uncommitted implementation based on that revision. Representative session
files (from the ordinary-value mean case) record each source checksum and the
common environment in `weighted-summaries-before-session.txt` and
`weighted-summaries-after-session.txt`.

For 1,000 observations by 1,000 draws, ordinary-value means and medians reduced
peak vector-heap growth from about 32.1 MB to 24.1 MB. Ordinary-value MAD,
variance, and standard deviation stayed near 44.1 MB; removing the repeated
8 MB double matrix does not necessarily reduce the garbage-collector high-water
mark. Full-rvec cases were essentially unchanged. Timings are indicative only.

## Logical-predicate comparison

`logical-predicates.csv` compares `84872e8` with the uncommitted implementation
based on that revision. Representative session files from `is.finite` record
source checksums and the common environment in
`logical-predicates-before-session.txt` and
`logical-predicates-after-session.txt`.

For a 1,000 by 1,000 logical rvec, peak vector-heap growth for `is.nan`,
`is.finite`, and `is.infinite` fell from about 16.0 MB to 4.0 MB. Allocation
tracing of a separate warmed call likewise fell from four approximately 4 MB
allocations to one: the three integer-conversion temporaries are eliminated,
leaving the logical result. The allocation metric includes only allocations
exceeding 1,000,000 bytes. The `is.na` control stayed at about 4.0 MB; existing
constructor fast paths already avoid extra matrix copies there. Timings are
indicative only.

## Logical-math comparison

`logical-math.csv` compares `1b8c0cf` with the uncommitted implementation based
on that revision. Representative session files from `abs` record the math
source checksum and common environment in `logical-math-before-session.txt`
and `logical-math-after-session.txt`.

For a 1,000 by 1,000 logical rvec, peak vector-heap growth fell from about
16.0 MB to 8.0 MB for `abs` and `cumsum`, from 20.0 MB to 12.0 MB for `sqrt`,
and from 16.1 MB to 8.1 MB for `sum`. Allocation tracing of a separate warmed
call confirmed removal of two approximately 4 MB conversion temporaries.
The allocation metric includes only allocations exceeding 1,000,000 bytes.
The `is.finite` control stayed at about 4.0 MB. Timings are indicative only.

## Summary-dispatch comparison

`summary-dispatch.csv` compares `65bd57b` with the uncommitted implementation
based on that revision. Representative session files from the double-input
`sum` case record the math source checksum and common environment in
`summary-dispatch-before-session.txt` and `summary-dispatch-after-session.txt`.

For one unnamed 1,000 by 1,000 rvec, peak vector-heap growth decreased by
about 8.1 MB for double inputs and 4.1 MB for integer and logical inputs across
all four summaries. Double `sum` dropped from about 8.1 MB to 0.03 MB; logical
`sum` dropped from about 8.1 MB to 4.1 MB because its integer conversion remains.
`prod` retains its existing numerical algorithm and additional allocations.

Tracing a separate warmed call confirmed removal of one full input-matrix
allocation (8 MB double, 4 MB integer/logical). The allocation metric only
counts allocations exceeding 1,000,000 bytes; zero does not mean the call
allocates nothing. Timings are indicative only.
